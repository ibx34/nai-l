use std::{collections::HashMap, ptr::{self, null_mut}};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMArrayType, LLVMArrayType2, LLVMBuildAlloca, LLVMBuildCall2, LLVMBuildGEP2, LLVMBuildLoad2, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildStore, LLVMConstInt, LLVMConstNull, LLVMConstString, LLVMConstStringInContext, LLVMContextCreate, LLVMCreateBasicBlockInContext, LLVMCreateBuilderInContext, LLVMFunctionType, LLVMGetElementType, LLVMGetPointerAddressSpace, LLVMGetTypeByName2, LLVMGetTypeContext, LLVMInt32Type, LLVMInt32TypeInContext, LLVMInt8Type, LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPointerTypeInContext, LLVMPointerTypeIsOpaque, LLVMPositionBuilderAtEnd, LLVMPrintModuleToFile, LLVMSetIsInBounds, LLVMTypeOf, LLVMVoidType
    }, prelude::{
        LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
    }, transforms::pass_builder::LLVMOpaquePassBuilderOptions, LLVMContext, LLVMOpcode::LLVMGetElementPtr
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

pub struct CodeGen {
    pub cursor: Cursor<Node>,
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
}

impl CodeGen {
    pub unsafe fn init(nodes: Vec<Node>) -> Self {
        let context = LLVMContextCreate();
        let builder = LLVMCreateBuilderInContext(context);
        let module = LLVMModuleCreateWithNameInContext(cstr!("main"), context);
        
        let printf_ty = LLVMFunctionType(LLVMInt32Type(),  [].as_mut_ptr(), 0, 0);
        let printf = LLVMAddFunction(module, cstr!("printf"), printf_ty);

        let cursor = Cursor::init(nodes);
        let entry_ty = LLVMFunctionType(LLVMInt32Type(), [].as_mut_ptr(), 0, 0);
        let entry = LLVMAddFunction(module, cstr!("main"), entry_ty);
        let entry_block = LLVMAppendBasicBlock(entry, cstr!("entry"));
        LLVMPositionBuilderAtEnd(builder, entry_block);

        let ptr_type = LLVMPointerTypeInContext(context, 0);
        let alloc = LLVMBuildAlloca(builder, ptr_type, cstr!("str_ptr"));
        let original_val = "Hello world!\n\0";
        let value = LLVMConstStringInContext(
            context,
            original_val.as_bytes().as_ptr() as *const i8,
            std::mem::size_of_val(original_val.as_bytes()) as u32,
            1,
        );

        let mut indices = [LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0)];
        let built_gep = LLVMBuildGEP2(builder, ptr_type, alloc, indices.as_mut_ptr(), 1, cstr!("gep"));
        LLVMSetIsInBounds(built_gep, 1);
        // let loaded_str: *mut llvm_sys::LLVMValue = LLVMBuildGEP2(builder, int8_ptr_ty_for_char, alloc, [LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), 0, 0)].as_mut_ptr(), 2, cstr!(""));
        LLVMBuildStore(builder, built_gep, alloc);
        println!("{:?}", LLVMPointerTypeIsOpaque(LLVMTypeOf(alloc)));
        let loaded_str: *mut llvm_sys::LLVMValue = LLVMBuildLoad2(builder, LLVMPointerType(LLVMInt8TypeInContext(context), 0), alloc, cstr!("loaded_str"));
        LLVMBuildStore(builder, value, alloc);

        LLVMBuildCall2(builder, printf_ty, printf, [loaded_str].as_mut_ptr(), 1, cstr!("call_printf"));

        LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 0, 0));
        
        LLVMPrintModuleToFile(
            module,
            std::ffi::CString::new("out2.ll").unwrap().as_ptr(),
            null_mut(),
        );

        todo!();
        Self {
            cursor,
            context,
            builder,
        }
    }

    // pub unsafe fn generate(&mut self) -> Result<CodeGenRes, String> {
    //     todo!()
    // }

    // pub unsafe fn generate_all(&mut self) -> Result<CodeGenRes, String> {
    //     while let Some(node) = self.cursor.current() {
    //         println!("!");
    //         let node = node.to_owned();
    //         todo!()
    //     }
    //     Ok(CodeGenRes::AllGood)
    // }

    // pub unsafe fn print(&self) {
    //     LLVMPrintModuleToFile(
    //         self.modules.get("main").unwrap().inner_module,
    //         std::ffi::CString::new("out.ll").unwrap().as_ptr(),
    //         null_mut(),
    //     );
    // }
}
