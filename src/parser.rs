use crate::term::*;

// LEXER
// This is basically Rust syntax
#[derive(Copy, Clone, Debug, PartialEq)]
enum Tok<'a> {
    // x
    Name(&'a str),
    // fn
    Fn,
    // i32
    I32,
    // i64
    I64,
    // (
    OpenParen,
    // )
    CloseParen,
    // {
    OpenBrace,
    // }
    CloseBrace,
    // :
    Colon,
    // =
    Equals,
    // ;
    Semicolon,
    // ,
    Comma,
}
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}
impl<'a> Lexer<'a> {
    fn char(&self) -> Option<char> {
        self.input.as_bytes().get(self.pos).copied().map(char::from)
    }

    fn alpha(&mut self) -> Spanned<Tok<'a>> {
        let start = self.pos;
        while self.char().map_or(false, char::is_alphanumeric) {
            self.pos += 1;
        }
        let name = &self.input[start..self.pos];
        let tok = match name {
            "fn" => Tok::Fn,
            "i32" => Tok::I32,
            "i64" => Tok::I64,
            _ => Tok::Name(name),
        };
        Spanned::new(tok, Span(start, self.pos))
    }

    fn single<T>(&mut self, tok: Tok<'a>) -> Option<Result<Spanned<Tok<'a>>, T>> {
        self.pos += 1;
        Some(Ok(Spanned::new(tok, Span(self.pos - 1, self.pos))))
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Tok<'a>>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.char()? {
            '(' => self.single(Tok::OpenParen),
            ')' => self.single(Tok::CloseParen),
            '{' => self.single(Tok::OpenBrace),
            '}' => self.single(Tok::CloseBrace),
            ':' => self.single(Tok::Colon),
            ';' => self.single(Tok::Semicolon),
            '=' => self.single(Tok::Equals),
            ',' => self.single(Tok::Comma),
            x if x.is_whitespace() => {
                self.pos += 1;
                self.next()
            }
            x if x.is_alphabetic() => Some(Ok(self.alpha())),
            x => Some(Err(Spanned::new(
                format!("unrecognized token '{}'", x),
                Span(self.pos, self.pos + 1),
            ))),
        }
    }
}

// PARSER
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    next: Option<Spanned<Tok<'a>>>,
    next_err: Option<Error>,
}
impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer { input, pos: 0 },
            next: None,
            next_err: None,
        }
    }

    fn peek(&mut self) -> Option<Spanned<Tok<'a>>> {
        match self.next {
            Some(x) => Some(x),
            None => {
                if self.next_err.is_some() {
                    return None;
                }
                if let Some(x) = self.lexer.next() {
                    match x {
                        Ok(x) => {
                            self.next = Some(x);
                            // eprintln!("Lexing {:?}", x);
                            Some(x)
                        }
                        Err(e) => {
                            self.next_err = Some(e);
                            None
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    fn next(&mut self) -> Option<Spanned<Tok<'a>>> {
        let next = self.peek();
        self.next = None;
        next
    }

    fn name(&mut self) -> Option<Spanned<Name>> {
        match *self.peek()? {
            Tok::Name(x) => {
                let t = self.next()?;
                Some(Spanned::new(x.into(), t.span))
            }
            _ => None,
        }
    }

    fn term(&mut self) -> Option<STerm> {
        match *self.peek()? {
            Tok::Name(_) => {
                let name = self.name()?;
                let var = Box::new(Spanned::new(Term::Var(name.inner), name.span));
                match self.peek().as_deref() {
                    Some(Tok::OpenParen) => {
                        self.next();
                        let mut args = Vec::new();
                        while self.peek().as_deref() != Some(&Tok::CloseParen) {
                            let x = self.term()?;
                            args.push(x);
                            match self.peek().as_deref() {
                                Some(Tok::Comma) => {
                                    self.next();
                                }
                                Some(Tok::CloseParen) => (),
                                _ => todo!("error"),
                            }
                        }
                        match self.next() {
                            Some(Spanned {
                                inner: Tok::CloseParen,
                                span,
                            }) => Some(Box::new(Spanned::new(
                                Term::Call(var.clone(), args),
                                Span(var.span.0, span.1),
                            ))),
                            None => todo!("error: eof"),
                            _ => unreachable!(),
                        }
                    }
                    _ => Some(var),
                }
            }
            Tok::OpenParen => todo!("binop probably"),
            Tok::OpenBrace => {
                // block
                let start = self.lexer.pos;
                self.next();
                let mut block = Vec::new();
                if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                    return Some(Box::new(Spanned::new(
                        Term::Block(Vec::new(), None),
                        Span(start, self.lexer.pos),
                    )));
                }
                loop {
                    let x = self.stmt()?;
                    match (x, self.peek().as_deref()) {
                        (x @ Statement::Item(_), Some(Tok::CloseBrace)) => {
                            block.push(x);
                            self.next();
                            return Some(Box::new(Spanned::new(
                                Term::Block(block, None),
                                Span(start, self.lexer.pos),
                            )));
                        }
                        (x @ Statement::Item(_), _) => {
                            block.push(x);
                        }
                        (x @ Statement::Term(_), Some(Tok::Semicolon)) => {
                            block.push(x);
                            self.next();
                            if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                                self.next();
                                return Some(Box::new(Spanned::new(
                                    Term::Block(block, None),
                                    Span(start, self.lexer.pos),
                                )));
                            }
                        }
                        (Statement::Term(x), Some(Tok::CloseBrace)) => {
                            self.next();
                            return Some(Box::new(Spanned::new(
                                Term::Block(block, Some(x)),
                                Span(start, self.lexer.pos),
                            )));
                        }
                        _ => todo!("error"),
                    }
                }
            }
            _ => None,
        }
    }

    fn ty(&mut self) -> Option<Type> {
        match self.peek().as_deref()? {
            Tok::I32 => {
                self.next();
                Some(Type::I32)
            }
            Tok::I64 => {
                self.next();
                Some(Type::I64)
            }
            Tok::OpenParen => {
                self.next();
                assert_eq!(Some(&Tok::CloseParen), self.next().as_deref());
                Some(Type::Unit)
            }
            _ => None,
        }
    }

    fn item(&mut self) -> Option<Item> {
        match *self.peek()? {
            Tok::Fn => {
                // fn f(x: T, y: T): Z = x
                self.next();
                let name = self.name()?;
                // TODO errors
                assert_eq!(self.next().as_deref(), Some(&Tok::OpenParen));
                let mut args = Vec::new();
                while self.peek().as_deref() != Some(&Tok::CloseParen) {
                    // TODO these shouldn't be ? because they should catch errors
                    // if something has already captured input, it can't just return None
                    // maybe somehow enforce that? change types?
                    let n = self.name()?;
                    assert_eq!(self.next().as_deref(), Some(&Tok::Colon));
                    let t = self.ty()?;
                    args.push((n.inner, t));
                    match self.peek().as_deref() {
                        Some(Tok::Comma) => {
                            self.next();
                        }
                        Some(Tok::CloseParen) => (),
                        _ => todo!("error"),
                    }
                }
                assert_eq!(self.next().as_deref(), Some(&Tok::CloseParen));
                let ret_type = if let Some(&Tok::Colon) = self.peek().as_deref() {
                    self.next();
                    self.ty()?
                } else {
                    Type::Unit
                };

                let body = match self.peek().as_deref() {
                    Some(Tok::Equals) => {
                        self.next();
                        let t = self.term()?;
                        assert_eq!(self.next().as_deref(), Some(&Tok::Semicolon));
                        t
                    }
                    // let term() consume the brace
                    Some(Tok::OpenBrace) => self.term()?,
                    _ => todo!("error"),
                };

                Some(Item::Fn(Fn {
                    name,
                    ret_type,
                    args,
                    body: body.inner,
                }))
            }
            _ => None,
        }
    }

    fn stmt(&mut self) -> Option<Statement> {
        self.item()
            .map(Statement::Item)
            .or_else(|| self.term().map(Statement::Term))
    }

    pub fn top_level(&mut self) -> Result<Vec<Statement>, Error> {
        let mut v = Vec::new();
        while self.peek().is_some() {
            match self.stmt() {
                Some(x) => v.push(x),
                None => {
                    if let Some(x) = self.next_err.take() {
                        return Err(x);
                    } else {
                        return Err(Spanned::new(
                            format!("Unexpected {:?}, expected statement", self.peek().unwrap()),
                            Span(self.lexer.pos, self.lexer.pos + 1),
                        ));
                    }
                }
            }
        }
        match self.next_err.take() {
            Some(x) => Err(x),
            None => Ok(v),
        }
    }
}
