#![feature(let_chains)]
#![feature(iter_advance_by)]
#![feature(box_into_inner)]
use std::{
    borrow::{Borrow, Cow},
    collections::{binary_heap::PeekMut, HashMap},
    f32::consts::E,
    fmt::Debug,
    iter::Peekable,
    process::id,
};

#[derive(Debug, PartialEq, Eq)]
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
}

pub struct ASTParse<'a, A>
where
    A: Iterator<Item = &'a AstItem<'a>> + Debug,
{
    pub ast: Peekable<A>,
    pub ret: Vec<Expr>,
    // TODO: This will be useful later
    pub r#ref: HashMap<String, ()>
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

    pub fn parse_identifier(&mut self) -> Option<Expr> {
        if let Some(identifier) = self.ast.peek()
            && let AstItem::Identifier(ident) = identifier.to_owned()
        {
            assert!(self.ast.next().is_some());
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
        let assignment_ty = self.parse_identifier()?;
        if self.expect_following(vec![AstItem::Eq], true)
            && let Some(peeked) = self.ast.peek()
        {
            let peeked = peeked.to_owned();
            let Some(right) = self.parse_expr(peeked) else {
                return None;
            };

            let assignment = Expr::Assignment {
                visibility: (),
                typed: Box::new(assignment_ty),
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
                    return self.parse_identifier();
                }
                assert!(self.ast.next().is_some());
                // We know that it will be an assignment because right now that
                // is the only thing that has two colons that follow an identifier
                if self.expect_following(vec![AstItem::Colon, AstItem::Colon], true) {
                    let pa = self.parse_assignment(a);
                    return pa;
                }
                None
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
                    self.ret.push(ret);
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
    println!("{:?}\n\n", ast.ret);
    let mut parser = ASTParse {
        ast: ast.ret.iter().peekable(),
        ret: Vec::new(),
        r#ref: HashMap::new()
    };
    parser.parse_all();
    println!("{:?}", parser.ret);
}
