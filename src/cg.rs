use std::collections::HashMap;

use llvm_sys::{LLVMContext, core::{LLVMContextCreate, LLVMModuleCreateWithNameInContext}, prelude::{LLVMContextRef, LLVMModuleRef}};

use crate::Node;


macro_rules! cstr {
    ($s:expr) =>{ 
        std::ffi::CString::new($s).unwrap().as_ptr()
    };
}

#[derive(Debug)]
pub struct Cursor<T> {
    pub idx: usize,
    pub len: usize,
    pub interior: Vec<T>
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
        return self.interior.get(self.idx)
    }
}

pub struct Module<'a> {
    inner_module: LLVMModuleRef,
    pub name: &'a str,
}

impl<'a> Module<'a> {
    pub unsafe fn new_named(ctx: LLVMContextRef, name: &'a str) -> Self {
        let inner_module = LLVMModuleCreateWithNameInContext(cstr!(name), ctx);
        Module {
            inner_module,
            name
        }
    }
}

pub struct CodeGen<'a> {
    pub cursor: Cursor<Node>,
    pub modules: HashMap<&'a str, Module<'a>>,
    pub context: LLVMContextRef
}

impl<'a> CodeGen<'a> {
    pub unsafe fn init(nodes: Vec<Node>) -> Self {
        let context = LLVMContextCreate();
        let main = Module::new_named(context, "main");
        let mut modules = HashMap::new();
        modules.insert("main", main);

        let mut cursor = Cursor::init(nodes);
        // remove when you feel like handling functions in LLVM
        cursor.next();

        Self {
            cursor,
            modules,
            context
        }
    }
    pub fn generate(&mut self) {
        while let Some(node) = self.cursor.peek() {
            println!("{node:?}");
            if self.cursor.next().is_none() {
                break;
            }
        }
    }
}