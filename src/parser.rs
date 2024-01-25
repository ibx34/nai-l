use std::{collections::HashMap, hash::Hash, net::UdpSocket};

use llvm_sys::orc2::ee;

use crate::{utils::Cursor, AstItem, Keywords, AST};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    StringLiteral(String),
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
        right: Box<Expr>,
    },
    ModulePath {
        segmants: Vec<Box<Expr>>,
    },
    FunctionCall {
        to_call: Box<Expr>,
        arguments: Vec<Box<Expr>>,
        long_form: bool,
    },
    TypeParam {
        has_name: Option<Box<Expr>>,
        ret: Option<Box<Expr>>,
    },
    LetIn {
        let_what: Box<Expr>,
        be_in: Box<Expr>
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftSideOfFunctionAssignment {
    pub name: Box<Expr>,
    pub type_list: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub ast: Cursor<&'a AstItem<'a>>,
    pub ret: Vec<Node>,
    pub current_indent_level: usize,
}

impl<'a> Parser<'a> {
    pub fn init(ast: Vec<&'a AstItem<'a>>) -> Self {
        Self {
            ast: Cursor::init(ast),
            ret: Vec::new(),
            current_indent_level: 0,
        }
    }

    pub fn parse_all(&mut self) {
        while let Some(next) = self.ast.current() {
            let next = next.to_owned();
            match self.parse_expr(next) {
                Some(parsed) => self.ret.push(Node::Expr(parsed.to_owned())),
                _ => {}
            }
        }
    }

    pub fn parse_function_call(&mut self, current: &'a AstItem) -> Option<Expr> {
        match current {
            // long form
            AstItem::OpenParenthesis => {
                let next = self.ast.next().unwrap().to_owned();
                let to_call = self.parse_expr(next)?;
                if !matches!(to_call, Expr::Identifier(_) | Expr::ModulePath { .. }) {
                    panic!("Expected either an identifier or module path after the first parenthesis.")
                }
                
                let mut collected: Vec<Box<Expr>> = Vec::new();
                while let Some(current) = self.ast.current() {
                    let current = current.to_owned();
                    if let AstItem::CloseParenthesis = current {
                        self.ast.next();
                        break;
                    } else if let AstItem::LessThan = current {
                        if matches!(self.ast.next()?, AstItem::Dash) {
                            self.ast.next();
                            continue;
                        }
                    } else {
                        let Some(parsed) = self.parse_expr(current) else {
                            println!("{:?} didnt make the cut for {:?}", current, to_call);
                            break;
                        };
                        collected.push(Box::new(parsed));
                        continue;
                    }
                }
                println!("Calling {:?} with the arguments {:?}", to_call, collected);
                return Some(Expr::FunctionCall {
                    to_call: Box::new(to_call),
                    arguments: collected,
                    long_form: true,
                });
            }
            _ => todo!(),
        }
    }

    pub fn parse_assignment(&mut self, identifier: Expr, pass_argument_check: bool, skip_type_list: bool) -> Option<Expr> {
        let mut type_list: Vec<Box<Expr>> = Vec::new();
        if !skip_type_list {
            while let Some(current) = self.ast.current() {
                let current = current.to_owned();
                if let AstItem::Eq = current {
                    self.ast.next();
                    break;
                } else if let AstItem::Dash = current {
                    self.ast.next();
                    println!("Dash?");
                    if let Some(AstItem::GreaterThan) = self.ast.current() {
                        self.ast.next();
                        continue;
                    }
                } else {
                    let Some(parsed) = self.parse_expr(current) else {
                        break;
                    };
                    if let Expr::Identifier(identifier) = parsed {
                        let Some(peeked) = self.ast.current() else {
                            break;
                        };
                        let peeked = peeked.to_owned();
                        if matches!(peeked, AstItem::Identifier(_)) {
                            type_list.push(Box::new(Expr::TypeParam {
                                has_name: Some(Box::new(Expr::Identifier(identifier.to_owned()))),
                                ret: self.parse_expr(peeked).map(|e| Box::new(e)),
                            }));
                            continue;
                        }
                        type_list.push(Box::new(Expr::TypeParam {
                            has_name: None,
                            ret: Some(Box::new(Expr::Identifier(identifier))),
                        }));
                    } else if let a @ Expr::ModulePath { .. } = parsed {
                        type_list.push(Box::new(Expr::TypeParam {
                            has_name: None,
                            ret: Some(Box::new(a)),
                        }));
                    }
                }
            }
        }
        if let Some(more) = self.ast.current() {
            let more = more.to_owned();
  
            let Some(right_side) = self.parse_expr(more) else {
                panic!("Failed to get right side of assignment to {:?} ({:?}) ({:?},{:?})", identifier, more, pass_argument_check, skip_type_list);
            };
            if pass_argument_check || (type_list.len() > 1
                || type_list
                    .iter()
                    .filter(|e| {
                        if let Expr::TypeParam { has_name, .. } = *(*(e.to_owned())).to_owned() {
                            has_name.is_some()
                        } else {
                            false
                        }
                    })
                    .collect::<Vec<&Box<Expr>>>()
                    .len()
                    > 0)
            {
                return Some(Expr::FunctionAssignment {
                    visibility: (),
                    left: LeftSideOfFunctionAssignment {
                        name: Box::new(identifier),
                        type_list,
                    },
                    right: Box::new(right_side),
                });
            }
            let Expr::TypeParam { ret, .. } = *(type_list.first().unwrap().to_owned()) else {
                panic!("!!");
            };
            return Some(Expr::Assignment {
                visibility: (),
                typed: Box::new(*ret.unwrap()),
                left: Box::new(identifier),
                right: Box::new(right_side),
            });
        }
        panic!("no more for us to use :(");
    }

    pub fn parse_expr(&mut self, parse: &'a AstItem<'a>) -> Option<Expr> {
        match parse {
            a @ AstItem::OpenParenthesis => self.parse_function_call(a),
            a @ AstItem::Identifier(identifier) | a @ AstItem::UseOfProtectedIdentifier(identifier) => {
                let ident = identifier.to_string();
                let protected = matches!(a, AstItem::UseOfProtectedIdentifier(_)) && crate::RESERVED_KEYWORDS.contains(&ident.as_str());
                let pass_argument_check = if protected {
                    // this checks hould pull from a static/const vec of protected identifiers, not just main. temporary.
                    true
                } else {
                    false
                };
                match self.ast.peek() {
                    Some(AstItem::Colon) => {
                        if self.ast.advance_by(2) {
                            if let Some(AstItem::Colon) = self.ast.current() {
                                self.ast.next();
                                let parsed_assignment = self.parse_assignment(Expr::Identifier(ident), pass_argument_check, false)?;
                                // if matches!(self.ast.current(), Some(AstItem::Keyword(Keywords::In))) {
                                //     self.ret.push(Node::Expr(parsed_assignment));
                                //     return Some(self.parse_expr(self.ast.current()?)?);
                                // }
                                return Some(parsed_assignment)
                            }
                        }
                        return None;
                    }
                    Some(AstItem::Dot) => {
                        self.ast.next();
                        let mut segmants: Vec<_> = Vec::from(&[Box::new(Expr::Identifier(ident))]);
                        while let Some(current) = self.ast.current() {
                            let current = current.to_owned();
                            match current {
                                AstItem::Dot => _ = self.ast.next(),
                                a @ AstItem::Identifier(_) => {
                                    if let Some(b @ Expr::Identifier(_)) = self.parse_expr(a) {
                                        segmants.push(Box::new(b));
                                        continue;
                                    }
                                    break;
                                }
                                _ => break,
                            }
                        }
                        return Some(Expr::ModulePath { segmants });
                    }
                    Some(AstItem::Eq) if protected => {
                        // In some cases (like the protected identifier) there may not be any 
                        // types and such we should handle this function as kind of a "assignment"
                        self.ast.advance_by(2);
                        return self.parse_assignment(Expr::Identifier(ident), pass_argument_check, true);
                    }
                    _ => {
                        self.ast.next();
                        return Some(Expr::Identifier(ident));
                    }
                }
            }
            AstItem::String(str) => {
                self.ast.next();
                return Some(Expr::StringLiteral(str.to_string()));
            }
            AstItem::Keyword(Keywords::Let) => {
                self.ast.next();
                let let_what = Box::new(self.parse_expr(self.ast.current()?)?);
                if !matches!(self.ast.current()?, AstItem::Keyword(Keywords::In)) {
                    panic!("Expected an 'in' {:?}", self.ast.current());
                }
                self.ast.next();
                let be_in = Box::new(self.parse_expr(self.ast.current()?)?);
                return Some(Expr::LetIn { let_what, be_in });
            }
            _ => {
                self.ast.next();
                return None;
            }
        }
    }
}
