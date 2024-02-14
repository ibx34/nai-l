use std::{
    collections::HashMap,
    hash::Hash,
    ptr::{self, null_mut},
    sync::Arc,
};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMArrayType, LLVMArrayType2, LLVMBuildAlloca,
        LLVMBuildCall2, LLVMBuildGEP2, LLVMBuildLoad2, LLVMBuildRet, LLVMBuildRetVoid,
        LLVMBuildStore, LLVMConstInt, LLVMConstNull, LLVMConstString, LLVMConstStringInContext,
        LLVMContextCreate, LLVMCreateBasicBlockInContext, LLVMCreateBuilderInContext,
        LLVMFunctionType, LLVMGetElementType, LLVMGetParam, LLVMGetParams,
        LLVMGetPointerAddressSpace, LLVMGetTypeByName2, LLVMGetTypeContext, LLVMInt32Type,
        LLVMInt32TypeInContext, LLVMInt8Type, LLVMInt8TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPointerTypeInContext,
        LLVMPointerTypeIsOpaque, LLVMPositionBuilderAtEnd, LLVMPrintModuleToFile,
        LLVMSetIsInBounds, LLVMSetValueName2, LLVMTypeOf, LLVMVoidType,
    },
    prelude::{
        LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
    },
    transforms::pass_builder::LLVMOpaquePassBuilderOptions,
    LLVMContext,
    LLVMOpcode::LLVMGetElementPtr,
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

#[derive(Debug)]
pub struct DefinedFunction<'a> {
    pub name: &'a str,
    pub params: Vec<DefinedFunctionParam>,
    pub ret: LLVMTypeRef,
    pub r#fn: LLVMValueRef,
    pub entry: LLVMBasicBlockRef,
}

#[derive(Debug)]
pub struct Module<'a> {
    inner_module: LLVMModuleRef,
    functions: HashMap<&'a str, LLVMValueRef>,
    types: HashMap<&'a str, TypeDef<'a>>,
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

pub struct CodeGenOptions {
    pub allow_no_main_func: bool,
}

pub enum GenerateContext<'a> {
    Standalone,
    InFunction(DefinedFunction<'a>),
}

pub struct CodeGen<'a> {
    pub cursor: Cursor<Node>,
    pub opts: CodeGenOptions,
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub modules: HashMap<&'a str, Module<'a>>,
    pub cmod: &'a str,
}

pub enum GetModuleItemContext {
    Function,
    Type,
    Mixed(Vec<Box<GetModuleItemContext>>),
}

pub enum ModuleItemLookupRes<'a> {
    Function(LLVMValueRef),
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
        let printf_ty = LLVMFunctionType(LLVMInt32Type(), [].as_mut_ptr(), 0, 0);
        let printf = LLVMAddFunction(std_module.inner_module, cstr!("printf"), printf_ty);
        std_module.functions.insert("printf", printf);

        let main_module = Module::init(context, builder, "main");
        modules.insert("main", main_module);
        modules.insert("std", std_module);

        let cursor = Cursor::init(nodes);

        // let entry_ty = LLVMFunctionType(LLVMInt32Type(), [].as_mut_ptr(), 0, 0);
        // let entry = LLVMAddFunction(module, cstr!("main"), entry_ty);
        // let entry_block = LLVMAppendBasicBlock(entry, cstr!("entry"));
        // LLVMPositionBuilderAtEnd(builder, entry_block);

        // let ptr_type = LLVMPointerTypeInContext(context, 0);
        // let alloc = LLVMBuildAlloca(builder, ptr_type, cstr!("str_ptr"));
        // let original_val = "Hello world!\n\0";
        // let value = LLVMConstStringInContext(
        //     context,
        //     original_val.as_bytes().as_ptr() as *const i8,
        //     std::mem::size_of_val(original_val.as_bytes()) as u32,
        //     1,
        // );

        // let mut indices = [LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0)];
        // let built_gep = LLVMBuildGEP2(builder, ptr_type, alloc, indices.as_mut_ptr(), 1, cstr!("gep"));
        // LLVMSetIsInBounds(built_gep, 1);
        // // let loaded_str: *mut llvm_sys::LLVMValue = LLVMBuildGEP2(builder, int8_ptr_ty_for_char, alloc, [LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), 0, 0)].as_mut_ptr(), 2, cstr!(""));
        // LLVMBuildStore(builder, built_gep, alloc);
        // println!("{:?}", LLVMPointerTypeIsOpaque(LLVMTypeOf(alloc)));
        // let loaded_str: *mut llvm_sys::LLVMValue = LLVMBuildLoad2(builder, LLVMPointerType(LLVMInt8TypeInContext(context), 0), alloc, cstr!("loaded_str"));
        // LLVMBuildStore(builder, value, alloc);

        // LLVMBuildCall2(builder, printf_ty, printf, [loaded_str].as_mut_ptr(), 1, cstr!("call_printf"));

        // LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, 0));

        // LLVMPrintModuleToFile(
        //     module,
        //     std::ffi::CString::new("out2.ll").unwrap().as_ptr(),
        //     null_mut(),
        // );

        // todo!();
        Self {
            cursor,
            context,
            builder,
            modules,
            opts,
            cmod: "main",
        }
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
                return Ok(ModuleItemLookupRes::Function(module_func.to_owned()));
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

    pub unsafe fn generate(&mut self, node: Node, ctx: GenerateContext) -> Result<(), String> {
        match node {
            Node::Expr(Expr::Identifier(ident)) => {
                let GenerateContext::InFunction(DefinedFunction {
                    name,
                    params,
                    ret,
                    r#fn,
                    entry,
                }) = ctx
                else {
                    panic!("Expected a the InFunction variant of GenerateContext");
                };
                // This case handles identifiers in a function. First it looks at the params.
                let matching_params = params
                    .into_iter()
                    .find(|e| e.name.is_some() && e.name.as_ref().unwrap() == &ident);
                if let Some(matching_params) = matching_params {
                    LLVMPositionBuilderAtEnd(self.builder, entry);
                    LLVMBuildRet(self.builder, LLVMGetParam(r#fn, 0));
                    return Ok(())
                }
            }
            Node::Expr(Expr::FunctionAssignment {
                visibility,
                left,
                right,
            }) => {
                let mut cmod = self.get_current_module().inner_module;
                let func_name = CodeGen::get_identifier(*(left.name.to_owned()));
                let mut func_types = left
                    .type_list
                    .to_owned()
                    .iter()
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
                let ret_type: *mut llvm_sys::LLVMType = func_types.pop().unwrap().ty;
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
                let new_func = LLVMAddFunction(cmod, cstr!(func_name.as_bytes()), new_func_ty);

                let name = b"named";
                LLVMSetValueName2(LLVMGetParam(new_func, 0), name.as_ptr() as *const i8, name.len());

                let entry = LLVMAppendBasicBlock(new_func, cstr!("entry"));
                self.print();

                self.cursor.next();
                _ = self.generate(
                    Node::Expr(*(right.to_owned())),
                    GenerateContext::InFunction(DefinedFunction {
                        name: &func_name,
                        params: func_types,
                        ret: ret_type,
                        r#fn: new_func,
                        entry,
                    }),
                )?
            }
            _ => todo!(),
        }
        Ok(())
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
