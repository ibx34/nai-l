#![feature(let_chains)]
#![allow(temporary_cstring_as_ptr)]
#![feature(iter_advance_by)]
#![feature(box_into_inner)]
pub mod cg;
pub mod parser;
pub mod utils;

use llvm_sys::core::{
    LLVMContextCreate, LLVMCreateBuilderInContext, LLVMModuleCreateWithNameInContext,
    LLVMPrintModuleToFile,
};
use parser::LeftSideOfFunctionAssignment;
use std::{
    borrow::{Borrow, Cow},
    collections::{binary_heap::PeekMut, HashMap},
    f32::consts::E,
    fmt::Debug,
    iter::Peekable,
    path,
    process::id,
    ptr::null_mut,
    thread::panicking,
};

use crate::parser::{Expr, Node};

const RESERVED_KEYWORDS: [&str; 2] = ["extern", "module"];

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseTyRes {
    pub ret: Option<Expr>,
    pub named: Option<Expr>,
}

pub struct ASTParse<'a, A>
where
    A: Iterator<Item = &'a AstItem<'a>> + Debug + Clone,
{
    pub ast: Peekable<A>,
    pub ret: Vec<Node>,
    pub r#ref: HashMap<String, ()>,
}

impl<'a, A> ASTParse<'a, A>
where
    A: Iterator<Item = &'a AstItem<'a>> + Debug + Clone,
{
    pub fn advance_by(&mut self, n: usize) {
        assert!(self.ast.advance_by(n).is_ok())
    }
    /// Expects by PEEKING not advancing first
    pub fn expect_following(
        &mut self,
        expectants: Vec<AstItem<'a>>,
        advance: bool,
        advance_if_matches: bool,
    ) -> bool {
        for e in expectants {
            if let Some(n) = self.ast.peek() {
                if e != **n {
                    if advance && !advance_if_matches {
                        assert!(self.ast.next().is_some());
                    }
                    return false;
                }
                if advance && advance_if_matches {
                    assert!(self.ast.next().is_some());
                }
            }
        }
        true
    }

    pub fn parse_type(&mut self, p_res: Option<ParseTyRes>) -> ParseTyRes {
        let mut res = ParseTyRes {
            ret: None,
            named: None,
        };
        if let Some(peeked) = self.ast.peek() {
            let peeked = peeked.to_owned();
            let Some(l_expr) = self.parse_expr(peeked) else {
                panic!("Failed to parse expression???");
            };

            // this means we are in a function definition!!!
            if let Expr::Identifier(ident) = l_expr {
                if let Some(previois_res) = p_res
                    && previois_res.named.is_some()
                {
                    res.ret = Some(Expr::Identifier(ident));
                    res.named = previois_res.named;
                    return res;
                } else {
                    res.named = Some(Expr::Identifier(ident));
                    let parse_type_again_res = self.parse_type(Some(res.to_owned())).ret;
                    res.ret = parse_type_again_res;
                }
            } else if let module_path @ Expr::ModulePath { .. } = l_expr {
                res.ret = Some(module_path)
            }
        }
        return res;
    }

    pub fn parse_identifier(&mut self) -> Option<Expr> {
        let peeked = self.ast.peek();
        if let Some(identifier) = peeked
            && let AstItem::Identifier(ident) = identifier.to_owned()
        {
            return Some(Expr::Identifier(ident.to_string()));
        }
        None
    }

    pub fn parse_assignment(&mut self, identifier: &'a AstItem<'a>) -> Option<Expr> {
        // Check if the passed identifier is actually an identifier. Otherwise
        // raise left hand side exception
        let _ident @ AstItem::Identifier(ident) = identifier else {
            return None;
        };
        let assignment_ty: ParseTyRes = self.parse_type(None);
        let mut func_types = Vec::from(&[assignment_ty.to_owned()]);
        let arrow = self.expect_following(vec![AstItem::Dash, AstItem::GreaterThan], true, true);
        if arrow {
            while let Some(c) = self.ast.peek() {
                if *c == &AstItem::Eq {
                    println!("- HIT EQ!");
                    break;
                }
                self.expect_following(vec![AstItem::Dash, AstItem::GreaterThan], true, true);
                func_types.push(self.parse_type(None));
            }
        }
        if self.expect_following(vec![AstItem::Eq], true, true)
            && let Some(peeked) = self.ast.peek()
        {
            let peeked = peeked.to_owned();
            let Some(right) = self.parse_expr(peeked) else {
                return None;
            };

            if func_types.len() > 1 {
                // return Some(Expr::FunctionAssignment {
                //     visibility: (),
                //     left: LeftSideOfFunctionAssignment {
                //         name: Box::new(Expr::Identifier(ident.to_string())),
                //         // type_list: func_types,
                //     },
                //     right: Box::new(right),
                // });
                todo!()
            }

            let assignment = Expr::Assignment {
                visibility: (),
                typed: Box::new(assignment_ty.ret?),
                left: Box::new(Expr::Identifier(ident.to_string())),
                right: Box::new(right),
            };
            self.r#ref.insert(ident.to_string(), ());
            self.ast.next();
            return Some(assignment);
        }
        None
    }

    pub fn parse_expr(&mut self, item: &'a AstItem<'a>) -> Option<Expr> {
        let ret = match item {
            AstItem::String(string) => return Some(Expr::Identifier(string.to_string())),
            a @ AstItem::Identifier(ident) => {
                self.ast.next();
                match self.ast.peek() {
                    Some(AstItem::Dot) => {
                        let mut segmants =
                            Vec::from(&[Box::new(Expr::Identifier(ident.to_string()))]);
                        assert!(self.ast.next().is_some());
                        while let Some(peeked) = self.ast.peek() {
                            let peeked = peeked.to_owned();
                            match peeked {
                                AstItem::Identifier(identifier) => {
                                    segmants
                                        .push(Box::new(Expr::Identifier(identifier.to_string())));
                                    self.ast.next();
                                }
                                AstItem::Dot => _ = self.ast.next(),
                                a @ _ => {
                                    println!("DASH? {:?}", a);
                                    break;
                                }
                            }
                        }
                        Some(Expr::ModulePath { segmants })
                    }
                    Some(AstItem::Colon) => {
                        self.ast.next();
                        if let Some(AstItem::Colon) = self.ast.peek() {
                            self.ast.next();
                            self.parse_assignment(a)
                        } else {
                            panic!("Uknown colon case");
                        }
                    }
                    _ => {
                        Some(Expr::Identifier(ident.to_string()))
                    },
                }
            }
            _a @ _ => {
                self.ast.next();
                None
            }
        };
        return ret;
    }

    pub fn parse_all(&mut self) {
        while let Some(nai) = self.ast.peek() {
            let nai = nai.to_owned();
            match self.parse_expr(nai) {
                Some(ret) => {
                    self.ret.push(Node::Expr(ret));
                }
                None => {}
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstItem<'a> {
    Junk(Option<char>),
    Plus,
    Dash,
    GreaterThan,
    LessThan,
    Dot,
    Eq,
    Colon,
    Identifier(Cow<'a, str>),
    String(Cow<'a, str>),
}

pub struct AST<'a, A>
where
    A: Iterator<Item = char> + Debug,
{
    pub input: Peekable<A>,
    pub ret: Vec<AstItem<'a>>,
}

impl<'a, A> AST<'a, A>
where
    A: Iterator<Item = char> + Debug,
{
    pub fn advance_peek(&mut self) -> Option<&char> {
        assert!(self.input.next().is_some());
        self.input.peek()
    }
    pub fn collect_temp_string_till(
        &mut self,
        end: Vec<char>,
        start_with: Option<char>,
        treat_as_identifier: bool,
    ) -> Option<String> {
        let mut temp_str = if let Some(start_with) = start_with {
            assert!(self.input.next().is_some());
            String::from(start_with)
        } else {
            String::new()
        };
        while let Some(nc) = self.input.peek() {
            if end.contains(nc) || (treat_as_identifier && !nc.is_alphanumeric() && nc != &'_') {
                break;
            }
            temp_str.push(*nc);
            assert!(self.input.next().is_some());
        }
        return Some(temp_str);
    }
    pub fn push_back(&mut self, ast_item: AstItem<'a>) -> Option<AstItem<'a>> {
        assert!(self.input.next().is_some());
        return Some(ast_item);
    }
    pub fn determine(&mut self, to_determine: char) -> Option<AstItem<'a>> {
        match to_determine {
            '/' => {
                if let Some(peeked) = self.input.next()
                    && peeked == '/'
                {
                    assert!(self.input.next().is_some());
                    while let Some(c) = self.input.peek() {
                        if c == &'\n' {
                            break;
                        }
                        assert!(self.input.next().is_some());
                    }
                }
                return self.push_back(AstItem::Junk(None));
            }
            '+' => self.push_back(AstItem::Plus),
            '>' => self.push_back(AstItem::GreaterThan),
            '<' => self.push_back(AstItem::LessThan),
            '-' => self.push_back(AstItem::Dash),
            '.' => self.push_back(AstItem::Dot),
            '=' => self.push_back(AstItem::Eq),
            ':' => self.push_back(AstItem::Colon),
            a @ ' ' | a @ '\n' => return self.push_back(AstItem::Junk(Some(a))),
            '"' => {
                assert!(self.input.next().is_some());
                let temp_str = self
                    .collect_temp_string_till(vec!['"'], None, false)
                    .unwrap();
                assert!(self.input.next().is_some());
                return Some(AstItem::String(Cow::Owned(temp_str)));
            }
            current_c @ _ => {
                let temp_str = self
                    .collect_temp_string_till(vec![' ', '\n'], Some(current_c), true)
                    .unwrap();
                return Some(AstItem::Identifier(Cow::Owned(temp_str)));
            }
        }
    }
    pub fn determine_all(&mut self) {
        while let Some(nc) = self.input.peek() {
            let nc = nc.to_owned();
            match self.determine(nc) {
                // Discard the junk items. The entire system could be reworked to be way better but who cares. Maybe use a Result type instead of Option but who knows im not that smart
                //Some(a @ AstItem::Junk(Some(' '))) => self.ret.push(a),
                Some(AstItem::Junk(_)) => {}

                Some(a @ _) => self.ret.push(a),
                _ => panic!(),
            }
        }
    }
}

fn main() {
    let test_file = std::fs::read_to_string("./test.sk").unwrap();
    let chars = test_file.chars().peekable();

    let mut ast = AST {
        input: chars,
        ret: Vec::new(),
    };
    ast.determine_all();
    println!("{:?}", ast.ret);
    let mut parser = parser::Parser::init(ast.ret.iter().collect::<Vec<&AstItem<'_>>>());
    parser.parse_all();
    println!("{:#?}", parser.ret);
    // let mut parser = ASTParse {
    //     ast: ast.ret.iter().peekable(),
    //     ret: Vec::new(),
    //     r#ref: refs,
    // };
    // parser.parse_all();
    // println!("{:#?}", parser.ret);
    // unsafe {
    //     let mut cg = CodeGen::init(parser.ret);
    //     cg.generate_all();
    //     // let context = LLVMContextCreate();
    //     // let module = LLVMModuleCreateWithNameInContext(b"sum\0".as_ptr() as *const _, context);
    //     // let builder = LLVMCreateBuilderInContext(context);

    //     LLVMPrintModuleToFile(
    //         cg.modules.get("std").unwrap().inner_module,
    //         std::ffi::CString::new("out.ll").unwrap().as_ptr(),
    //         null_mut(),
    //     );
    // }
}
