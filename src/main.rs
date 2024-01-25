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

use crate::{
    cg::CodeGen,
    parser::{Expr, Node},
};

const RESERVED_KEYWORDS: [&str; 3] = ["main", "in", "let"];

#[derive(Debug, PartialEq, Eq)]
pub enum Keywords {
    In,
    Let
}

impl TryFrom<String> for Keywords {
    type Error = ();
    fn try_from(value: String) -> Result<Self, ()> {
        Ok(match value.as_str() {
            "in" => Keywords::In,
            "let" => Keywords::Let,
            _ => return Err(())  
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Indent {
    Increase,
    Decrease,
    NoContest
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
    OpenParenthesis,
    CloseParenthesis,
    OpenSquare,
    CloseSquare,
    Colon,
    Identifier(Cow<'a, str>),
    UseOfProtectedIdentifier(Cow<'a, str>),
    String(Cow<'a, str>),
    Keyword(Keywords),
    // This is = to \n + space ...
    Indent(Indent)
}

pub struct AST<'a, A>
where
    A: Iterator<Item = char> + Debug,
{
    pub input: Peekable<A>,
    pub ret: Vec<AstItem<'a>>,
    pub using_protected_identifier: bool,
    pub last_space_or_tab_count: usize
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
            '(' => self.push_back(AstItem::OpenParenthesis),
            ')' => self.push_back(AstItem::CloseParenthesis),
            '[' => self.push_back(AstItem::OpenSquare),
            ']' => self.push_back(AstItem::CloseSquare),
            // '\n' => {
            //     self.input.next();
            //     let mut count = 0;
            //     while let Some(peeked) = self.input.peek() {
            //         if peeked != &' ' && peeked != &'\t' {
            //             break;
            //         }
            //         self.input.next();
            //         count += 1;
            //     }
            //     return Some(AstItem::Indent(if count > self.last_space_or_tab_count {
            //         Indent::Increase  
            //     } else if  count < self.last_space_or_tab_count {
            //         Indent::Decrease
            //     } else if count == self.last_space_or_tab_count {
            //         Indent::NoContest
            //     } else {
            //         panic!("Shouldnt be possible");
            //     }));

            // }
            '🦀' => {
                assert!(self.input.next().is_some());
                let Some(next) = self.input.peek() else {
                    return None
                };
                let next=next.to_owned();
                self.using_protected_identifier = true;
                let AstItem::Identifier(ident) = self.determine(next)? else {
                    panic!("Expected an identifier to follow the crab🦀.")
                };
                return Some(AstItem::UseOfProtectedIdentifier(ident))
            }
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
                if RESERVED_KEYWORDS.contains(&temp_str.as_str()) && !self.using_protected_identifier {
                    // handle this dingus
                    self.using_protected_identifier = false;
                    return Some(AstItem::Keyword(Keywords::try_from(temp_str).unwrap()))
                }
                self.using_protected_identifier = false;
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
        using_protected_identifier: false,
        last_space_or_tab_count: 0
    };
    ast.determine_all();
    println!("{:#?}", ast.ret);
    let mut parser = parser::Parser::init(ast.ret.iter().collect::<Vec<&AstItem<'_>>>());
    parser.parse_all();
    println!("{:#?}", parser.ret);

    // unsafe {
    //     let mut cg = CodeGen::init(parser.ret);
    //     cg.generate_all().unwrap();
    //     cg.print();
    // }
}
