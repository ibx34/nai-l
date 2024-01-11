use std::collections::HashMap;

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMContextCreate, LLVMCreateBuilderInContext, LLVMFunctionType,
        LLVMInt32Type, LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMVoidType,
    },
    prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef},
    LLVMContext,
};

use crate::{Expr, Node};

macro_rules! cstr {
    ($s:expr) => {
        std::ffi::CString::new($s).unwrap().as_ptr()
    };
}

#[derive(Debug)]
pub struct Cursor<T> {
    pub idx: usize,
    pub len: usize,
    pub interior: Vec<T>,
}

impl<T> Cursor<T> {
    pub fn init(interior: Vec<T>) -> Self {
        Self {
            idx: 0,
            len: interior.len(),
            interior,
        }
    }
    pub fn next(&mut self) -> Option<&T> {
        self.idx += 1;
        if self.idx == 0 {
            return self.interior.first();
        }
        return self.interior.get(self.idx - 1);
    }
    pub fn peek(&mut self) -> Option<&T> {
        // TODO: Fix why this is how we peek funky
        return self.interior.get(self.idx);
    }
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
        }
    }

    pub fn get_inner_identifier(expr: Expr) -> Option<String> {
        if let Expr::Identifier(ident) = expr {
            return Some(ident);
        }
        None
    }

    pub fn visit_ty(&mut self, ty: Box<Expr>) -> Option<LLVMTypeRef> {
        let Some(possible_type) = CodeGen::get_inner_identifier(*(ty.to_owned())) else {
            return None;
        };
        let split_up = possible_type.split("::").collect::<Vec<&str>>();
        // its reasonable to expect the last item in the vector will be the actual item we want.
        let Some(type_name) = split_up.last() else {
            return None;
        };
        let Some(module_name) = split_up.first() else {
            return None;
        };
        let Some(module) = self.modules.get(*module_name) else {
            return None;
        };
        let Some(ty) = module.types.get(*type_name) else {
            return None;
        };
        return Some(*ty);
    }

    pub fn generate(&mut self, expr: Node) -> Option<()> {
        match expr {
            Node::Expr(Expr::Assignment {
                visibility,
                typed,
                left,
                right,
            }) => {
                let assignment_ty = self.visit_ty(typed);
                let left_side_identifier =
                    CodeGen::get_inner_identifier(*(left.to_owned())).unwrap();

                /*

                                let mut value = left.to_owned();
                value.push('\0');

                let alloc = LLVMBuildAlloca(
                    self.builder,
                    LLVMArrayType(
                        LLVMInt8Type(),
                        std::mem::size_of_val(value.as_bytes()) as u32,
                    ),
                    cstr!(""),
                );
                string
                 */

                // let llvm_type = LLVMCreateType
                return Some(());
            }
            _ => Some(()),
        }
    }

    pub fn generate_all(&mut self) {
        while let Some(node) = self.cursor.peek() {
            let node = node.to_owned();
            self.generate(node);
            if self.cursor.next().is_none() {
                break;
            }
        }
    }
}
