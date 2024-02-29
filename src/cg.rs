use std::{
    clone,
    collections::HashMap,
    hash::Hash,
    ptr::{self, null_mut},
    sync::Arc,
};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAddIncoming, LLVMAppendBasicBlock, LLVMAppendBasicBlockInContext,
        LLVMArrayType, LLVMArrayType2, LLVMBuildAlloca, LLVMBuildBr, LLVMBuildCall2, LLVMBuildGEP2,
        LLVMBuildLoad2, LLVMBuildPhi, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildStore, LLVMConstInt,
        LLVMConstNull, LLVMConstString, LLVMConstStringInContext, LLVMContextCreate,
        LLVMCreateBasicBlockInContext, LLVMCreateBuilderInContext, LLVMFunctionType,
        LLVMGetElementType, LLVMGetFirstInstruction, LLVMGetParam, LLVMGetParams,
        LLVMGetPointerAddressSpace, LLVMGetTypeByName2, LLVMGetTypeContext, LLVMGetTypeKind,
        LLVMInt32Type, LLVMInt32TypeInContext, LLVMInt8Type, LLVMInt8TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPointerTypeInContext,
        LLVMPointerTypeIsOpaque, LLVMPositionBuilderAtEnd, LLVMPositionBuilderBefore,
        LLVMPrintModuleToFile, LLVMSetIsInBounds, LLVMSetValueName2, LLVMTypeOf, LLVMVoidType,
    },
    prelude::{
        LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
    },
    transforms::pass_builder::LLVMOpaquePassBuilderOptions,
    LLVMContext,
    LLVMOpcode::LLVMGetElementPtr,
    LLVMTypeKind,
};

use crate::{
    parser::{Expr, Node},
    utils::Cursor,
};

macro_rules! cstr {
    ($s:expr) => {
        std::ffi::CString::new($s).unwrap().as_ptr()
    };
}

#[derive(Debug, Clone)]
pub struct DefinedFunctionParam {
    pub name: Option<String>,
    pub ty: LLVMTypeRef,
}

#[derive(Debug, Clone)]
pub enum TypeWrapper {
    Pointer(LLVMTypeRef),
    Str(LLVMTypeRef),
}

#[derive(Debug, Clone)]
pub struct DefinedFunction<'a> {
    pub name: &'a str,
    pub params: Vec<DefinedFunctionParam>,
    pub ret: LLVMTypeRef,
    pub r#fn: LLVMValueRef,
    pub entry: Option<LLVMBasicBlockRef>,
}

#[derive(Debug, Clone)]
pub struct DefinedAssignment<'a> {
    pub name: &'a str,
    pub ty: LLVMTypeRef,
}

#[derive(Debug)]
pub struct GenerateResultLLVMValue {
    pub val: LLVMValueRef,
    pub ty: LLVMTypeRef,
}

#[derive(Debug)]
pub struct Module<'a> {
    inner_module: LLVMModuleRef,
    pub functions: HashMap<String, (LLVMValueRef, LLVMTypeRef)>,
    pub types: HashMap<&'a str, TypeDef<'a>>,
    pub name: &'a str,
}

impl<'a> Module<'a> {
    fn init(ctx: LLVMContextRef, builder: LLVMBuilderRef, name: &'a str) -> Self {
        let inner_module = unsafe { LLVMModuleCreateWithNameInContext(cstr!(name), ctx) };
        Self {
            name,
            inner_module,
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct GetModuleNameRes(String, String);

#[derive(Debug)]
pub struct TypeDef<'a> {
    pub name: &'a str,
    pub inner: LLVMTypeRef,
    pub inner_ptr_type: Option<LLVMTypeRef>,
    pub visibility: (),
}

#[derive(Clone, Debug)]
pub struct CodeGenOptions {
    pub allow_no_main_func: bool,
}

#[derive(Clone)]
pub enum GenerateContext<'a> {
    Standalone,
    InFunction(DefinedFunction<'a>),
    InAssignment {
        func: Option<DefinedFunction<'a>>,
        assignment: DefinedAssignment<'a>,
    },
}

pub struct GenerateResult {
    pub val: Option<LLVMValueRef>,
    pub is_let_in: bool,
}

#[derive(Clone, Debug)]
pub struct BlockAssignment {
    pub value: LLVMValueRef,
    pub ty: LLVMTypeRef,
    pub original_val: Option<LLVMValueRef>,
    pub block_level: usize
}

#[derive(Clone, Debug)]
pub struct CurrentBlack {
    pub llvm: LLVMBasicBlockRef,
    pub assignments: HashMap<String, BlockAssignment>,
    pub nested_in: Vec<Box<CurrentBlack>>,
}

#[derive(Debug)]
pub struct CodeGen<'a> {
    pub cursor: Cursor<Node>,
    pub opts: CodeGenOptions,
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub modules: HashMap<&'a str, Module<'a>>,
    pub cmod: &'a str,
    pub cblock: Option<CurrentBlack>,
}

pub enum GetModuleItemContext {
    Function,
    Type,
    Mixed(Vec<Box<GetModuleItemContext>>),
}

pub enum ModuleItemLookupRes<'a> {
    Function((LLVMValueRef, LLVMTypeRef)),
    Type(&'a TypeDef<'a>),
}

impl<'a> CodeGen<'a> {
    pub unsafe fn init(nodes: Vec<Node>, opts: CodeGenOptions) -> Self {
        let context = LLVMContextCreate();
        let builder = LLVMCreateBuilderInContext(context);

        let mut modules = HashMap::new();

        // Standard module definition for the first generation compiler aka bootstrapped compiler. Over engineered? Probably.
        let mut std_module = Module::init(context, builder, "std");
        let str_ty = TypeDef {
            name: "str",
            inner: LLVMPointerTypeInContext(context, 0),
            inner_ptr_type: Some(LLVMPointerType(LLVMInt8TypeInContext(context), 0)),
            visibility: (),
        };
        std_module.types.insert("str", str_ty);
        let void_ty = TypeDef {
            name: "void",
            inner: LLVMVoidType(),
            inner_ptr_type: None,
            visibility: (),
        };
        std_module.types.insert("void", void_ty);

        let mut main_module = Module::init(context, builder, "main");
        let printf_ty = LLVMFunctionType(
            LLVMInt32Type(),
            [LLVMPointerType(LLVMInt8TypeInContext(context), 0)].as_mut_ptr(),
            1,
            1,
        );
        let printf = LLVMAddFunction(main_module.inner_module, cstr!("printf"), printf_ty);
        main_module
            .functions
            .insert("printf".to_string(), (printf, printf_ty));

        modules.insert("main", main_module);
        modules.insert("std", std_module);

        let cursor = Cursor::init(nodes);

        Self {
            cursor,
            context,
            builder,
            modules,
            opts,
            cmod: "main",
            cblock: None,
        }
    }
    pub fn get_current_block(&self) -> &CurrentBlack {
        self.cblock.as_ref().unwrap()
    }
    pub fn get_current_block_mut(&mut self) -> &mut CurrentBlack {
        self.cblock.as_mut().unwrap()
    }
    pub fn set_current_block(
        &mut self,
        block: LLVMBasicBlockRef,
        previous_block: Option<CurrentBlack>,
    ) -> Option<&CurrentBlack> {
        let nested_blocks = if let Some(cblock) = &self.cblock
            && let Some(prev) = previous_block
        {
            let mut nested = cblock.nested_in.clone();
            nested.push(Box::new(prev));
            nested
        } else {
            Vec::new()
        };

        let mut assignments = HashMap::new();
        for block in &nested_blocks {
            let block = *(block.to_owned());
            assignments.extend(block.assignments.into_iter());
        }
        self.cblock = Some(CurrentBlack {
            llvm: block,
            assignments,
            nested_in: nested_blocks,
        });
        self.cblock.as_ref()
    }
    pub fn get_identifier(expr: Expr) -> String {
        let Expr::Identifier(ident) = expr else {
            panic!("Not an identifier")
        };
        return ident;
    }
    pub fn get_module_item(
        &mut self,
        ctx: GetModuleItemContext,
        to_find: Expr,
    ) -> Result<ModuleItemLookupRes, ()> {
        let name = match to_find {
            Expr::Identifier(ref ident) => {
                GetModuleNameRes(self.cmod.to_string(), ident.to_owned())
            }
            Expr::ModulePath { segmants } => {
                let mut module_and_item = Vec::new();
                for segmant in segmants {
                    if let Expr::Identifier(identifier) = *(segmant.to_owned()) {
                        module_and_item.push(identifier);
                    }
                }
                let module_and_item = &module_and_item[0..2];
                GetModuleNameRes(module_and_item[0].to_owned(), module_and_item[1].to_owned())
            }
            a @ _ => panic!(
                "Expected an identifier or module path when trying to look up a module item. Got {:?}",
                a
            ),
        };
        let module = self.modules.get_mut(name.0.as_str()).unwrap();
        let to_find = name.1.as_str();
        match ctx {
            GetModuleItemContext::Function => {
                let Some(module_func) = module.functions.get(to_find) else {
                    panic!("Failed to find {to_find:?} in the module {:?}", module.name);
                };
                return Ok(ModuleItemLookupRes::Function((
                    module_func.to_owned().0,
                    module_func.to_owned().1,
                )));
            }
            GetModuleItemContext::Type => {
                let Some(ty) = module.types.get(to_find) else {
                    panic!("Failed to find {to_find:?} in the module {:?}", module.name);
                };
                return Ok(ModuleItemLookupRes::Type(ty));
            }
            _ => todo!(),
        }
    }

    pub fn get_current_module(&mut self) -> &mut Module<'a> {
        self.modules.get_mut(self.cmod).unwrap()
    }

    // todo: return some form of LLVMValue for the assignment branch when it generates
    // the right side expression so we can store the result of whatever that might be.
    pub unsafe fn generate(
        &mut self,
        node: Node,
        ctx: GenerateContext,
    ) -> Result<GenerateResult, String> {
        match node {
            Node::Expr(Expr::LetIn { let_what, be_in }) => {
                let GenerateContext::InFunction(DefinedFunction {
                    name,
                    params,
                    ret,
                    r#fn,
                    entry,
                }) = ctx.to_owned()
                else {
                    panic!("Expected a the InFunction variant of GenerateContext");
                };
                let where_to_go = if let Some(cblock) = &self.cblock {
                    cblock.llvm
                } else {
                    entry.unwrap()
                };
                LLVMPositionBuilderAtEnd(self.builder, where_to_go);
                // LetWhat can only be an assignment, for right now.
                let new_in_block = LLVMAppendBasicBlockInContext(self.context, r#fn, cstr!("br"));
                LLVMBuildBr(self.builder, new_in_block);
                LLVMPositionBuilderAtEnd(self.builder, new_in_block);
                let previous_block = self.cblock.to_owned();
                self.set_current_block(new_in_block, previous_block.to_owned());

                _ = self.generate(Node::Expr(*(let_what.to_owned())), ctx.to_owned())?;
                if let Some(cblock) = previous_block {
                    let first = LLVMGetFirstInstruction(new_in_block);
                    if !first.is_null() {
                        LLVMPositionBuilderBefore(self.builder, first);
                    }
                    let assignments_iter = cblock.assignments.to_owned();
                    for assignment in assignments_iter {
                        let kind = LLVMGetTypeKind(assignment.1.ty);
                        match kind {
                            llvm_sys::LLVMTypeKind::LLVMPointerTypeKind => {
                                let new_val = if assignment.1.block_level >= 1 {
                                    assignment.1.value
                                } else {
                                    LLVMBuildLoad2(
                                        self.builder,
                                        assignment.1.ty,
                                        assignment.1.value,
                                        cstr!(assignment.0.as_str()),
                                    )
                                };
                                let current_block = self.get_current_block_mut();
                                current_block.assignments.insert(
                                    assignment.0.to_owned(),
                                    BlockAssignment {
                                        value: new_val,
                                        ty: assignment.1.ty,
                                        original_val: Some(assignment.1.value),
                                        block_level: assignment.1.block_level + 1
                                    },
                                );
                            }
                            _ => todo!(),
                        }
                    }

                    println!("Updated ... {:#?}", self.cblock);
                    LLVMPositionBuilderAtEnd(self.builder, new_in_block);
                }
                let be_in: GenerateResult = self.generate(Node::Expr(*be_in.to_owned()), ctx)?;

                return Ok(GenerateResult {
                    val: Some(be_in.val.unwrap()),
                    is_let_in: true,
                });
            }
            Node::Expr(Expr::Assignment {
                visibility,
                typed,
                left,
                right,
            }) => {
                assert!(self.cblock.is_some());
                let builder = self.builder.to_owned();
                self.cursor.next();

                let left = CodeGen::get_identifier(*(left.to_owned()));
                let Ok(ModuleItemLookupRes::Type(r#type)) =
                    self.get_module_item(GetModuleItemContext::Type, *(typed.to_owned()))
                else {
                    panic!("Expected a type to be in the tpye list...");
                };
                let ty = r#type.inner.to_owned();
                let da = DefinedAssignment { name: &left, ty };
                let ctx = GenerateContext::InAssignment {
                    func: Some(match ctx {
                        GenerateContext::InFunction(ctx) => ctx,
                        _ => todo!(),
                    }),
                    assignment: da,
                };
                let ret = match self.generate(Node::Expr(*(right.to_owned())), ctx) {
                    Ok(GenerateResult { val, .. }) => val.unwrap(),
                    _ => todo!(),
                };

                let alloca: *mut llvm_sys::LLVMValue =
                    LLVMBuildAlloca(builder, ty, cstr!(left.to_owned()));
                LLVMBuildStore(builder, ret, alloca);
                let assignment = BlockAssignment { value: alloca, ty, original_val: None, block_level: 0 };

                let cblock = self.cblock.as_mut().unwrap();
                cblock.assignments.insert(left, assignment);

                return Ok(GenerateResult {
                    val: Some(ret),
                    is_let_in: false,
                });
            }
            Node::Expr(Expr::StringLiteral(lit)) => {
                let (name, inline) = match &ctx {
                    GenerateContext::InFunction(inf) => (inf.name, true),
                    GenerateContext::InAssignment { assignment, .. } => (assignment.name, false),
                    _ => panic!("Expected a different context."),
                };
                let ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                let alloc =
                    LLVMBuildAlloca(self.builder, ptr_type, cstr!(format!("{}_str_lit", name)));

                let val = LLVMConstStringInContext(
                    self.context,
                    lit.as_bytes().as_ptr() as *const i8,
                    std::mem::size_of_val(lit.as_bytes()) as u32,
                    0,
                );
                LLVMBuildStore(self.builder, val, alloc);

                let mut indices = [LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0)];
                let built_gep = LLVMBuildGEP2(
                    self.builder,
                    ptr_type,
                    alloc,
                    indices.as_mut_ptr(),
                    1,
                    cstr!(format!("{}_str_lit_gep", name)),
                );
                LLVMSetIsInBounds(built_gep, 1);

                return Ok(GenerateResult {
                    val: Some(alloc),
                    is_let_in: false,
                });
            }
            Node::Expr(Expr::FunctionCall {
                to_call,
                arguments,
                long_form,
            }) => {
                println!("{:?}", *(to_call.to_owned()));
                let name = match &ctx {
                    GenerateContext::InFunction(inf) => inf.name,
                    GenerateContext::InAssignment { assignment, .. } => assignment.name,
                    _ => panic!("Expected a different context."),
                };
                let name = &format!("{name}_call");

                let Ok(ModuleItemLookupRes::Function(func)) =
                    self.get_module_item(GetModuleItemContext::Function, *(to_call.to_owned()))
                else {
                    panic!("Couldnt find wthat function")
                };

                let mut args = arguments
                    .clone()
                    .into_iter()
                    .map(|e| {
                        let ret = self
                            .generate(Node::Expr(*(e.to_owned())), ctx.to_owned())
                            .unwrap();
                        match ret.val {
                            Some(val) => val,
                            None => panic!("WHAT??"),
                        }
                    })
                    .collect::<Vec<LLVMValueRef>>();

                LLVMPositionBuilderAtEnd(self.builder, self.get_current_block().llvm);

                let ret: *mut llvm_sys::LLVMValue = LLVMBuildCall2(
                    self.builder,
                    func.1,
                    func.0,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    cstr!(name.to_string()),
                );

                return Ok(GenerateResult {
                    val: Some(ret),
                    is_let_in: false,
                });
            }
            Node::Expr(Expr::Identifier(ident)) => match ctx {
                GenerateContext::InFunction(DefinedFunction {
                    name,
                    params,
                    ret,
                    r#fn,
                    entry,
                }) => {
                    let matching_params = params
                        .into_iter()
                        .enumerate()
                        .find(|e| e.1.name.is_some() && e.1.name.as_ref().unwrap() == &ident);
                    if let Some(matching_params) = matching_params
                        && let Some(entry) = entry
                    {
                        return Ok(GenerateResult {
                            val: Some(LLVMGetParam(r#fn, matching_params.0.try_into().unwrap())),
                            is_let_in: false,
                        });
                    } else if let Some(cblock) = &self.cblock
                        && let Some(in_block) = cblock.assignments.get(&ident)
                    {
                        let in_block = in_block.to_owned();
                        let kind = LLVMGetTypeKind(in_block.ty);

                        // let ret = match kind {
                        //     llvm_sys::LLVMTypeKind::LLVMPointerTypeKind => LLVMBuildLoad2(
                        //         self.builder,
                        //         in_block.ty,
                        //         in_block.value,
                        //         cstr!(format!("loaded_ret_for_identifier_{}", ident.to_string())),
                        //     ),
                        //     _ => in_block.value,
                        // };
                        println!("{:?} is assigned to {:?}", ident, in_block.value);
                        return Ok(GenerateResult {
                            val: Some(in_block.value),
                            is_let_in: false,
                        });
                    }
                }
                _ => unreachable!(),
            },
            Node::Expr(Expr::FunctionAssignment {
                visibility,
                left,
                right,
            }) => {
                let unkown_ret = LLVMInt32TypeInContext(self.context);
                let mut func_types = left
                    .type_list
                    .to_owned()
                    .into_iter()
                    .map(|e| {
                        let unboxed = *(e.to_owned());
                        let Expr::TypeParam { has_name, ret } = unboxed.to_owned() else {
                            panic!("Expected type param")
                        };
                        let Ok(ModuleItemLookupRes::Type(r#type)) = self.get_module_item(
                            GetModuleItemContext::Type,
                            *(ret.unwrap().to_owned()),
                        ) else {
                            panic!("Expected a type to be in the tpye list...");
                        };
                        let name = if let Some(has_name) = has_name {
                            let identifier = CodeGen::get_identifier(*(has_name.to_owned()));
                            Some(identifier)
                        } else {
                            None
                        };
                        DefinedFunctionParam {
                            name,
                            ty: r#type.inner,
                        }
                    })
                    .collect::<Vec<DefinedFunctionParam>>();

                let mut cmod = self.get_current_module();
                let mut cmod_inner = cmod.inner_module;
                let ret_type: *mut llvm_sys::LLVMType = func_types
                    .pop()
                    .unwrap_or(DefinedFunctionParam {
                        name: None,
                        ty: unkown_ret,
                    })
                    .ty;
                let mut only_llvmty = func_types
                    .to_owned()
                    .iter()
                    .map(|e| e.ty)
                    .collect::<Vec<LLVMTypeRef>>();
                let new_func_ty = LLVMFunctionType(
                    ret_type,
                    only_llvmty.as_mut_ptr(),
                    only_llvmty.len().try_into().unwrap(),
                    0,
                );

                let func_name = CodeGen::get_identifier(*(left.name.to_owned()));
                println!("{func_name:?} :: {:?}", LLVMGetTypeKind(ret_type));
                let new_func =
                    LLVMAddFunction(cmod_inner, cstr!(func_name.as_bytes()), new_func_ty);
                cmod.functions
                    .insert(func_name.to_owned(), (new_func, new_func_ty));

                for (idx, param_) in func_types.to_owned().into_iter().enumerate() {
                    if let Some(name) = param_.name {
                        let param = LLVMGetParam(new_func, idx.try_into().unwrap());
                        LLVMSetValueName2(param, name.as_ptr() as *const i8, name.len());
                    }
                }

                let entry = LLVMAppendBasicBlock(new_func, cstr!("entry"));
                LLVMPositionBuilderAtEnd(self.builder, entry);
                self.set_current_block(entry, None);
                self.cursor.next();
                let Some(right) = self
                    .generate(
                        Node::Expr(*(right.to_owned())),
                        GenerateContext::InFunction(DefinedFunction {
                            name: func_name.clone().as_str(),
                            params: func_types,
                            ret: ret_type,
                            r#fn: new_func,
                            entry: Some(entry),
                        }),
                    )?
                    .val
                else {
                    panic!("?");
                };
                LLVMBuildRet(self.builder, right);
                return Ok(GenerateResult {
                    val: None,
                    is_let_in: false,
                });
            }
            a @ _ => {
                println!("{a:?}");
                todo!()
            }
        }
        return Ok(GenerateResult {
            val: None,
            is_let_in: false,
        });
    }

    pub unsafe fn generate_all(&mut self) -> Result<(), String> {
        while let Some(node) = self.cursor.current() {
            let node = node.to_owned();
            self.generate(node, GenerateContext::Standalone).unwrap();
        }
        Ok(())
    }

    pub unsafe fn print(&self) {
        LLVMPrintModuleToFile(
            self.modules.get("main").unwrap().inner_module,
            std::ffi::CString::new("out.ll").unwrap().as_ptr(),
            null_mut(),
        );
    }
}
