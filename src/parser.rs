use crate::{AstItem, utils::Cursor, AST};

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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SupposedType {
    pub has_name: Option<Expr>,
    pub ret: Option<Expr>
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LeftSideOfFunctionAssignment {
    pub name: Box<Expr>,
    pub type_list: Vec<SupposedType>,
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub ast: Cursor<&'a AstItem<'a>>,
    pub ret: Vec<Node>
}

impl<'a> Parser<'a> {
    pub fn init(ast: Vec<&'a AstItem<'a>>) -> Self {
        Self {
            ast: Cursor::init(ast),
            ret: Vec::new()
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

    pub fn parse_assignment(&mut self, identifier: Expr) -> Option<Expr> {
        let Expr::Identifier(ident_str) = identifier else {
            return None;
        };
        let mut type_list = Vec::new();
        while let Some(peeked) = self.ast.current() {
            let peeked = peeked.to_owned();
            match peeked {
                AstItem::Dash => {
                    self.ast.next();
                    if let Some(AstItem::GreaterThan) = self.ast.current() {
                        self.ast.next();
                        continue;
                    }
                }
                AstItem::Eq => {
                    self.ast.next();
                    println!("EQ == {:?}", self.ast.current());
                    break;
                }
                a @ AstItem::Identifier(_) => {
                    // TODO: better control flow
                    match self.parse_expr(a) {
                        a @ Some(Expr::Identifier(_)) => {
                            if let Some(type_after) = self.parse_expr(self.ast.current().unwrap()) {
                                type_list.push(SupposedType {
                                    has_name: a,
                                    ret: Some(type_after)
                                })                
                            } else {
                                type_list.push(SupposedType {
                                    has_name: None,
                                    ret: a
                                })
                            }
                        },
                        a @ Some(Expr::ModulePath { .. }) => {
                            type_list.push(SupposedType {
                                has_name: None,
                                ret: a
                            });
                        }
                        _ => break,
                    }
                }
                AstItem::Junk(Some(' ')) => _ = self.ast.next(),
                a @ _ => {
                    self.ast.next();
                    continue;
                }
            }
        }
        println!("{type_list:#?}");
        let right_hand_side = self.parse_expr(self.ast.current().unwrap());
        if right_hand_side.is_some() && (type_list.len() > 1 || type_list.iter().filter(|e| e.has_name.is_some()).collect::<Vec<&SupposedType>>().len() > 0) {
            return Some(Expr::FunctionAssignment { visibility: (), left: LeftSideOfFunctionAssignment {
                name: Box::new(Expr::Identifier(ident_str)),
                type_list
            }, right: Box::new(right_hand_side.unwrap()) })
        }
        return Some(Expr::Assignment { visibility: (), typed: Box::new(type_list.get(0).unwrap().ret.as_ref().unwrap().to_owned()), left: Box::new(Expr::Identifier(ident_str)), right: Box::new(right_hand_side.unwrap()) })
    }

    pub fn parse_expr(&mut self, parse: &'a AstItem<'a>) -> Option<Expr> {
        match parse {
            AstItem::Identifier(identifier) => {
                let ident_string = identifier.to_string();
                // Advances past the top-level identifier, if there is one
                let Some(next) = self.ast.next() else {
                    // What else should we do? Panic? That would panic if the file ended in a identifier
                    return None
                };
                
                match next {
                    AstItem::Colon => {
                        if let Some(AstItem::Colon) = self.ast.next() {
                            self.ast.next();
                            return self.parse_assignment(Expr::Identifier(ident_string));
                        }
                        panic!("Expected another colon, tbh. {:?}", self.ast.current());
                    }
                    AstItem::Dot => {
                        let mut segmants = Vec::from(&[Box::new(Expr::Identifier(ident_string))]);
                        while let Some(n) = self.ast.peek() {
                            match n {
                                AstItem::Dot => {
                                    self.ast.next();
                                    continue;
                                },
                                AstItem::Identifier(ident) => {
                                    segmants.push(Box::new(Expr::Identifier(ident.to_string())));
                                    self.ast.next();

                                },
                                _ => break
                            }
                        }
                        self.ast.next();
                        return Some(Expr::ModulePath { segmants })
                    }
                    _ => {}
                }
                return Some(Expr::Identifier(ident_string))
            },
            AstItem::Junk(Some(' ')) => {
                self.ast.next();
                None
            },
            AstItem::String(str) => {
                self.ast.next();
                return Some(Expr::StringLiteral(str.to_string()))
            }
            _ => {
                self.ast.next();
                return None
            }
        }
    }
}