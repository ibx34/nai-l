#![feature(let_chains)]
#![allow(temporary_cstring_as_ptr)]
#![feature(iter_advance_by)]
#![feature(box_into_inner)]
pub mod cg;
use std::{
    borrow::{Borrow, Cow},
    collections::{binary_heap::PeekMut, HashMap},
    f32::consts::E,
    fmt::Debug,
    iter::Peekable,
    process::id,
    ptr::null_mut
};
use llvm_sys::core::{LLVMModuleCreateWithNameInContext,LLVMCreateBuilderInContext,LLVMContextCreate, LLVMPrintModuleToFile};

use crate::cg::{Cursor, CodeGen};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Expr(Expr)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Assignment {
        /// TODO: This currently doesnt matter. Future Proofing?
        visibility: (),
        // After the ::
        // Its not optional, really...
        typed: Box<Expr>,
        left: Box<Expr>,
        // After the =
        right: Box<Expr>,
    },
    /// Some SIMPLE SIMPLE SIMPLE!! rules for function delcaration
    /// To avoid it being confused for assignment--to the parser, not you--ALL functions
    /// must have at least ONE named type and ONE non-named type in its type list. 
    /// This stems from the idea that for it to be a function it must 
    ///     1. take some sort of input and
    ///     2. it must return some sort of output based on the input
    /// If it only meets #2 of the self-evident truths then it is just assigning a named variable
    FunctionAssignment {
        visibility: (),
        left: LeftSideOfFunctionAssignment,
        right: Box<Expr>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftSideOfFunctionAssignment {
    name: Box<Expr>,
    type_list: Vec<ParseTyRes>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseTyRes {
    pub identifier_pass: bool,
    pub ret: Option<Expr>,
    pub named_type_for_parameters: Option<Expr>,
    pub make_func: bool,
}

pub struct ASTParse<'a, A>
where
    A: Iterator<Item = &'a AstItem<'a>> + Debug,
{
    pub ast: Peekable<A>,
    pub ret: Vec<Node>,
    pub r#ref: HashMap<String, ()>,
}

impl<'a, A> ASTParse<'a, A>
where
    A: Iterator<Item = &'a AstItem<'a>> + Debug,
{
    pub fn advance_by(&mut self, n: usize) {
        assert!(self.ast.advance_by(n).is_ok())
    }
    /// Expects by PEEKING not advancing first
    pub fn expect_following(&mut self, expectants: Vec<AstItem<'a>>, advance: bool) -> bool {
        for e in expectants {
            if let Some(n) = self.ast.peek() {
                if e != **n {
                    return false;
                }
                if advance {
                    assert!(self.ast.next().is_some());
                }
            }
        }
        true
    }

    pub fn parse_type(&mut self) -> ParseTyRes {
        let mut res = ParseTyRes {
            identifier_pass: false,
            ret: None,
            named_type_for_parameters: None,
            make_func: false
        };

        let Some(Expr::Identifier(ident)) = self.parse_identifier() else {
            return res
        };
        assert!(self.ast.next().is_some());

        if ["String"].contains(&ident.to_string().as_ref()) {
            res.identifier_pass = true;
            res.ret = Some(Expr::Identifier(ident));
            return res
        }

        // Add to refs
        self.r#ref.insert(ident.to_string(), ());

        // TODO: Check if what is here is some special keyword or not
        res.make_func = true;
        res.named_type_for_parameters = Some(Expr::Identifier(ident));
        res.ret = self.parse_type().ret;
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
        let assignment_ty = self.parse_type();

        let mut func_types = Vec::from(&[assignment_ty.to_owned()]);
        let arrow = self.expect_following(vec![AstItem::Dash, AstItem::GreaterThan], true);
        if arrow {
            while let Some(c) = self.ast.peek() {
                if *c == &AstItem::Eq {
                    break;
                }
                // This is just to advance and expect? The result really doesnt matter for right now...
                self.expect_following(vec![AstItem::Dash, AstItem::GreaterThan], true);
                func_types.push(self.parse_type());
            }
        }

        if self.expect_following(vec![AstItem::Eq], true)
            && let Some(peeked) = self.ast.peek()
        {

            let peeked = peeked.to_owned();
            let Some(right) = self.parse_expr(peeked) else {
                return None;
            };

            if assignment_ty.make_func {
            
    
                    return Some(Expr::FunctionAssignment {
                        visibility: (),
                        left: LeftSideOfFunctionAssignment {
                            name: Box::new(Expr::Identifier(ident.to_string())),
                        type_list: func_types,
                        },
                        right: Box::new(right)
                    })
                
            }

            let assignment = Expr::Assignment {
                visibility: (),
                typed: Box::new(assignment_ty.ret?),
                left: Box::new(Expr::Identifier(ident.to_string())),
                right: Box::new(right),
            };
            self.r#ref.insert(ident.to_string(), ());
            return Some(assignment)
        }
        None
    }

    pub fn parse_expr(&mut self, item: &'a AstItem<'a>) -> Option<Expr> {
        match item {
            AstItem::String(string) => {
                return Some(Expr::Identifier(string.to_string()))
            },
            a @ AstItem::Identifier(ident) => {
                if self.r#ref.contains_key(&ident.to_string()) {
                    self.r#ref.remove(&ident.to_string());
                    return self.parse_identifier();
                }
                assert!(self.ast.next().is_some());
                // We know that it will be an assignment because right now that
                // is the only thing that has two colons that follow an identifier
                if self.expect_following(vec![AstItem::Colon, AstItem::Colon], true) {
                    let pa: Option<Expr> = self.parse_assignment(a);
                    return pa;
                }
                panic!("Temporary unknown variable {:?}", ident);
            }
            a @ _ => {
                assert!(self.ast.next().is_some());
                None
            }
        }
    }
    pub fn parse_all(&mut self) {
        while let Some(nai) = self.ast.peek() {
            let nai = nai.to_owned();
            match self.parse_expr(nai) {
                Some(ret) => {
                    self.ret.push(Node::Expr(ret));
                },
                None => {}
            }
            if self.ast.next().is_none() {
                break;
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
                return Some(AstItem::Junk(None));
            }
            '+' => return Some(AstItem::Plus),
            '>' => return Some(AstItem::GreaterThan),
            '<' => return Some(AstItem::LessThan),
            '-' => return Some(AstItem::Dash),
            '.' => return Some(AstItem::Dot),
            '=' => return Some(AstItem::Eq),
            ':' => return Some(AstItem::Colon),
            a @ ' ' | a @ '\n' => return Some(AstItem::Junk(Some(a))),
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
                Some(AstItem::Junk(_)) => {}
                Some(a @ _) => self.ret.push(a),
                _ => panic!(),
            }
            if self.input.next().is_none() {
                break;
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
    let mut refs: HashMap<String, ()> = HashMap::new();
    refs.insert(String::from("String"), ());

    let mut parser = ASTParse {
        ast: ast.ret.iter().peekable(),
        ret: Vec::new(),
        r#ref: HashMap::new(),
    };
    parser.parse_all();

    unsafe {
        let mut cg = CodeGen::init(parser.ret);
        cg.generate();
        // let context = LLVMContextCreate();
        // let module = LLVMModuleCreateWithNameInContext(b"sum\0".as_ptr() as *const _, context);
        // let builder = LLVMCreateBuilderInContext(context);

        // LLVMPrintModuleToFile(module, cstr!("cbt.ll"), null_mut());
    }
}
