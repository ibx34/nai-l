use std::{collections::HashMap, ptr::null_mut};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMContextCreate, LLVMCreateBuilderInContext, LLVMFunctionType,
        LLVMInt32Type, LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMVoidType, LLVMPrintModuleToFile,
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
    pub builder: LLVMBuilderRef,
}

impl<'a> Module<'a> {
    pub unsafe fn new_named(ctx: LLVMContextRef, builder: LLVMBuilderRef, name: &'a str) -> Self {
        let inner_module = LLVMModuleCreateWithNameInContext(cstr!(name), ctx);

        Module {
            inner_module,
            name,
            builder,
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

        let mut cursor = Cursor::init(nodes);
        // remove when you feel like handling functions in LLVM
        cursor.next();
        
        Self {
            cursor,
            modules,
            context,
            builder,
            // SAFETY: These two WILL exist.
            current_module: "main"
        }
    }

    pub fn get_current_module(&mut self) -> &mut Module<'a> {
        self.modules.get_mut(self.current_module).unwrap()
    }

    pub fn try_to_get_type(&self, module: &str, ty: &str) -> Option<LLVMTypeRef> {
        let Some(module) = self.modules.get(module) else {
            return None
        };
        let Some(ty) = module.types.get(ty) else {
            return None
        };
        Some(ty.to_owned())
    }

    pub fn get_inner_identifier(expr: Expr) -> Option<String> {
        if let Expr::Identifier(ident) = expr {
            return Some(ident);
        } 
        None
    }

    pub fn generate_type(&mut self, expr: Box<Expr>) -> Result<LLVMTypeRef, String> {
        let mut c_mod = self.get_current_module();
        match *expr {
            Expr::Identifier(ident) => {
                if let Some(possible_predefined_type) = c_mod.types.get(ident.as_str()) {
                    let pt = possible_predefined_type.to_owned();
                    return Ok(pt)
                }
                return Err(format!("Could not find the type {:?}", ident))
            },
            Expr::TypeParam { ret , ..} => {
                let Some(ret) = ret else {
                    return Err(String::from("Expected 'ret' to be Some"));
                };
                return Ok(self.generate_type(ret)?);
            },
            Expr::ModulePath { segmants } => {
                let all_unboxed = segmants.iter().map_while(|e: &Box<Expr>| Some(*(e.to_owned()))).collect::<Vec<Expr>>();
                let module = CodeGen::get_inner_identifier(all_unboxed.first().unwrap().to_owned()).unwrap();
                let expected_type = CodeGen::get_inner_identifier(all_unboxed.last().unwrap().to_owned()).unwrap();
                
                let Some(possible_type) = self.try_to_get_type(&module, &expected_type) else {
                    return Err(format!("Could not find {expected_type:?} in module {module:?}"));
                };
                return Ok(possible_type);
            }
            _ => return Err(String::from("Expected something else... TypeParam or Identifier"))
        }
    }   

    pub unsafe fn generate(&mut self, node: Node) -> Result<(), String> {
        match node {
            Node::Expr(expr) => match expr {
                Expr::Assignment { visibility, typed, left, right } => {
                    let at_type = self.generate_type(typed).unwrap();
                    Ok(())
                }
                _ => Ok(())
            },
        }
    }

    pub unsafe fn generate_all(&mut self) -> Result<(), String> {
        while let Some(node) = self.cursor.current() {
            let node = node.to_owned();
            self.generate(node)?;
            todo!()
        }
        Ok(())
    }

    pub unsafe fn print_ir(&'a self) {
        LLVMPrintModuleToFile(
            self.modules.get("std").unwrap().inner_module,
            std::ffi::CString::new("out.ll").unwrap().as_ptr(),
            null_mut(),
        );
    }
}
