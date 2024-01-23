use std::{collections::HashMap, ptr::null_mut};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMArrayType, LLVMArrayType2, LLVMBuildAlloca, LLVMBuildStore, LLVMConstString, LLVMContextCreate, LLVMCreateBasicBlockInContext, LLVMCreateBuilderInContext, LLVMFunctionType, LLVMGetTypeByName2, LLVMGetTypeContext, LLVMInt32Type, LLVMInt8Type, LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPositionBuilderAtEnd, LLVMPrintModuleToFile, LLVMVoidType
    },
    prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef},
    LLVMContext,
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

pub struct Module<'a> {
    pub(crate) inner_module: LLVMModuleRef,
    pub types: HashMap<&'a str, LLVMTypeRef>,
    pub functions: HashMap<&'a str, LLVMValueRef>,
    pub name: &'a str,
}

impl<'a> Module<'a> {
    pub unsafe fn new_named(ctx: LLVMContextRef, builder: LLVMBuilderRef, name: &'a str) -> Self {
        let inner_module = LLVMModuleCreateWithNameInContext(cstr!(name), ctx);

        Module {
            inner_module,
            name,
            functions: HashMap::new(),
            types: HashMap::new(),
        }
    }
    pub fn add_fn(&mut self, name: &'a str, func: LLVMValueRef) {
        self.functions.insert(name, func);
    }
}

pub unsafe fn create_std_module(ctx: LLVMContextRef, builder: LLVMBuilderRef) -> Module<'static> {
    let mut module = Module::new_named(ctx, builder, "std");

    let malloc_ty = LLVMFunctionType(
        LLVMPointerType(LLVMVoidType(), 1),
        [LLVMInt32Type()].as_mut_ptr(),
        1,
        0,
    );
    let malloc = LLVMAddFunction(module.inner_module, cstr!("malloc"), malloc_ty);
    module.add_fn("malloc", malloc);

    module
}

pub struct TypeHinting {
    /// Size of the assignment or whatever as a usize
    pub size: u32,
}

#[derive(Debug)]
pub enum NeedsWork {
    ConstString {
        val_ref: LLVMValueRef,
        size_hint: u32,
    },
}
#[derive(Debug)]
pub enum CodeGenRes {
    AllGood,
    NeedsWork(NeedsWork),
}

pub struct CodeGen<'a> {
    pub cursor: Cursor<Node>,
    pub modules: HashMap<&'a str, Module<'a>>,
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    pub current_module: &'a str,
}

impl<'a> CodeGen<'a> {
    pub unsafe fn init(nodes: Vec<Node>) -> Self {
        let context = LLVMContextCreate();
        let builder = LLVMCreateBuilderInContext(context);

        let main = Module::new_named(context, builder, "main");
        let std = create_std_module(context, builder);

        let mut modules = HashMap::new();
        modules.insert("main", main);
        // TODO: Directive to disable this
        modules.insert("std", std);

        let cursor = Cursor::init(nodes);

        Self {
            cursor,
            modules,
            context,
            builder,
            // SAFETY: These two WILL exist.
            current_module: "main",
        }
    }

    pub fn get_current_module(&mut self) -> &mut Module<'a> {
        self.modules.get_mut(self.current_module).unwrap()
    }

    pub fn try_to_get_type(&self, module: &str, ty: &str) -> Option<LLVMTypeRef> {
        let Some(module) = self.modules.get(module) else {
            return None;
        };
        let Some(ty) = module.types.get(ty) else {
            return None;
        };
        Some(ty.to_owned())
    }

    pub fn get_inner_identifier(expr: Expr) -> Option<String> {
        if let Expr::Identifier(ident) = expr {
            return Some(ident);
        }
        None
    }

    pub unsafe fn generate_type(
        &mut self,
        expr: Box<Expr>,
        type_hinting: Option<TypeHinting>,
    ) -> Result<(LLVMTypeRef, LLVMValueRef), String> {
        let mut c_mod: &mut Module<'_> = self.get_current_module();
        match *expr {
            Expr::Identifier(ident) => {
                if let Some(type_hinting) = type_hinting
                    && &ident == "str"
                {
                    let ty = LLVMArrayType2(LLVMInt8TypeInContext(self.context), type_hinting.size as u64);
                    let alloc =
                        LLVMBuildAlloca(self.builder, ty, cstr!(""));


                    return Ok((ty,alloc));
                } else {
                    if let Some(possible_predefined_type) = c_mod.types.get(ident.as_str()) {
                        let pt = possible_predefined_type.to_owned();
                        todo!();
                        //return Ok(pt);
                    }
                    return Err(format!("Could not find the type {:?}", ident));
                }
            }
            Expr::TypeParam { ret, .. } => {
                let Some(ret) = ret else {
                    return Err(String::from("Expected 'ret' to be Some"));
                };
                todo!();
                //return Ok(self.generate_type(ret, None)?);
            }
            Expr::ModulePath { segmants } => {
                let all_unboxed = segmants
                    .iter()
                    .map_while(|e: &Box<Expr>| Some(*(e.to_owned())))
                    .collect::<Vec<Expr>>();
                let module =
                    CodeGen::get_inner_identifier(all_unboxed.first().unwrap().to_owned()).unwrap();
                let expected_type =
                    CodeGen::get_inner_identifier(all_unboxed.last().unwrap().to_owned()).unwrap();

                let Some(possible_type) = self.try_to_get_type(&module, &expected_type) else {
                    return Err(format!(
                        "Could not find {expected_type:?} in module {module:?}"
                    ));
                };
                todo!()
                //return Ok(possible_type);
            }
            _ => {
                return Err(String::from(
                    "Expected something else... TypeParam or Identifier",
                ))
            }
        }
    }

    pub unsafe fn generate(&mut self, node: Node) -> Result<CodeGenRes, String> {
        match node {
            Node::Expr(expr) => match expr {
                Expr::FunctionAssignment {
                    visibility,
                    left,
                    right,
                } => {
                    let name_of_func = CodeGen::get_inner_identifier(*left.name).unwrap();
                    let guessed_return_type = if left.type_list.len() == 0 && &name_of_func == "main" {
                        // panic!("Function needs to have at least 2 arguments. One named, one return. The return argument is decided by whether or not it is named.");
                        LLVMVoidType()
                    } else {
                        left.type_list.iter().find_map(|e| {
                            let Expr::TypeParam { has_name, ret } = *(e.to_owned()) else {
                                return None;
                            };
                            let has_name = has_name.map(|e| *e);
                            if has_name.is_none() {
                                return Some(self.generate_type(ret?, None).unwrap());
                            }
                            None
                        }).unwrap().0
                    };

                    let c_mod: &mut Module<'_> = self.get_current_module();
                    let func_def_ty = LLVMFunctionType(guessed_return_type, [].as_mut_ptr(), 0, 0);
                    let func_def = LLVMAddFunction(c_mod.inner_module, cstr!(name_of_func), func_def_ty);

                    let blcok = LLVMAppendBasicBlock(func_def, cstr!("entry"));
                    LLVMPositionBuilderAtEnd(self.builder, blcok);

                    self.generate(Node::Expr(*(right.to_owned())))?;
                    self.cursor.next();
                    Ok(CodeGenRes::AllGood)
                }
                Expr::Assignment {
                    visibility,
                    typed,
                    left,
                    right,
                } => {
                    let assignment_name = CodeGen::get_inner_identifier(*left).unwrap();
                    let CodeGenRes::NeedsWork(NeedsWork::ConstString { val_ref, size_hint }) =
                        self.generate(Node::Expr(*right))?
                    else {
                        return Err(String::from("whattt"));
                    };

                    let at_type = self
                        .generate_type(typed, Some(TypeHinting { size: size_hint }))
                        .unwrap();

                    LLVMBuildStore(self.builder, val_ref, at_type.1);
                    Ok(CodeGenRes::AllGood)
                }
                Expr::StringLiteral(mut value) => {
                    value.push('\0');
                    let size = std::mem::size_of_val(value.as_bytes()) as u32;

                    return Ok(CodeGenRes::NeedsWork(NeedsWork::ConstString {
                        val_ref: LLVMConstString(
                            value.as_bytes().as_ptr() as *const i8,
                            std::mem::size_of_val(value.as_bytes()) as u32,
                            1,
                        ),
                        size_hint: size,
                    }));
                }
                _ => {
                    println!("other");
                    Ok(CodeGenRes::AllGood)
                },
            },
        }
    }

    pub unsafe fn generate_all(&mut self) -> Result<CodeGenRes, String> {
        while let Some(node) = self.cursor.current() {
            println!("!");
            let node = node.to_owned();
            self.generate(node).unwrap();
        }
        Ok(CodeGenRes::AllGood)
    }

    pub unsafe fn print(&'a self) {
        LLVMPrintModuleToFile(
            self.modules.get("main").unwrap().inner_module,
            std::ffi::CString::new("out.ll").unwrap().as_ptr(),
            null_mut(),
        );
    }
}
