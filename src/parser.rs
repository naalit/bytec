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
    // let
    Let,

    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,

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
            "let" => Tok::Let,
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
            '+' => self.single(Tok::Add),
            '-' => self.single(Tok::Sub),
            '*' => self.single(Tok::Mul),
            '/' => self.single(Tok::Div),

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
                Doc::start("unrecognized token '").add(x).add("'"),
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
    bindings: Bindings,
}
impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer { input, pos: 0 },
            next: None,
            next_err: None,
            bindings: Default::default(),
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

    fn err(&self, err: &(impl std::fmt::Display + ?Sized)) -> Error {
        Spanned::new(Doc::start(err), self.span())
    }

    fn span(&self) -> Span {
        Span(self.lexer.pos, self.lexer.pos + 1)
    }

    fn expect(&mut self, tok: Tok, msg: &str) -> Result<(), Error> {
        if self.next().as_deref() == Some(&tok) {
            Ok(())
        } else {
            Err(self.err(&format!("expected {}", msg)))
        }
    }

    fn name(&mut self) -> Option<Spanned<RawSym>> {
        match *self.peek()? {
            Tok::Name(x) => {
                let t = self.next()?;
                Some(Spanned::new(self.bindings.raw(x), t.span))
            }
            _ => None,
        }
    }

    fn term(&mut self) -> Result<Option<SPre>, Error> {
        let mut t = match self.factor()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            let op = match self.peek().as_deref() {
                Some(Tok::Add) => BinOp::Add,
                Some(Tok::Sub) => BinOp::Sub,
                _ => break,
            };

            self.next();

            let rhs = self.factor()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
    }

    fn factor(&mut self) -> Result<Option<SPre>, Error> {
        let mut t = match self.atom()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            let op = match self.peek().as_deref() {
                Some(Tok::Mul) => BinOp::Mul,
                Some(Tok::Div) => BinOp::Div,
                _ => break,
            };

            self.next();

            let rhs = self.atom()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
    }

    fn atom(&mut self) -> Result<Option<SPre>, Error> {
        match self.peek().as_deref() {
            None => return Ok(None),
            Some(Tok::Name(_)) => {
                let name = self.name().unwrap();
                let var = Box::new(Spanned::new(Pre::Var(name.inner), name.span));
                match self.peek().as_deref() {
                    Some(Tok::OpenParen) => {
                        self.next();
                        let mut args = Vec::new();
                        while self.peek().as_deref() != Some(&Tok::CloseParen) {
                            let x = self.term()?.ok_or(self.err("expected function argument"))?;
                            args.push(x);
                            match self.peek().as_deref() {
                                Some(Tok::Comma) => {
                                    self.next();
                                }
                                Some(Tok::CloseParen) => (),
                                _ => return Err(self.err("expected ',' or ')'")),
                            }
                        }
                        match self.next() {
                            Some(Spanned {
                                inner: Tok::CloseParen,
                                span,
                            }) => Ok(Some(Box::new(Spanned::new(
                                Pre::Call(name, args),
                                Span(var.span.0, span.1),
                            )))),
                            None => return Err(self.err("unclosed argument list, expected ')'")),
                            _ => unreachable!(),
                        }
                    }
                    _ => Ok(Some(var)),
                }
            }
            Some(Tok::OpenParen) => {
                self.next();
                let t = self.term()?;
                self.expect(Tok::CloseParen, "closing ')'")?;
                Ok(t)
            }
            Some(Tok::OpenBrace) => {
                // block
                let start = self.lexer.pos;
                self.next();
                let mut block = Vec::new();
                if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                    return Ok(Some(Box::new(Spanned::new(
                        Pre::Block(Vec::new(), None),
                        Span(start, self.lexer.pos),
                    ))));
                }
                loop {
                    let x = self.stmt()?.ok_or(self.err("expected statement"))?;
                    match (x, self.peek().as_deref()) {
                        (x @ PreStatement::Term(_), Some(Tok::Semicolon)) => {
                            block.push(x);
                            self.next();
                            if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                                self.next();
                                return Ok(Some(Box::new(Spanned::new(
                                    Pre::Block(block, None),
                                    Span(start, self.lexer.pos),
                                ))));
                            }
                        }
                        (PreStatement::Term(x), Some(Tok::CloseBrace)) => {
                            self.next();
                            return Ok(Some(Box::new(Spanned::new(
                                Pre::Block(block, Some(x)),
                                Span(start, self.lexer.pos),
                            ))));
                        }
                        (x, Some(Tok::CloseBrace)) => {
                            block.push(x);
                            self.next();
                            return Ok(Some(Box::new(Spanned::new(
                                Pre::Block(block, None),
                                Span(start, self.lexer.pos),
                            ))));
                        }
                        (x, _) => {
                            block.push(x);
                        }
                    }
                }
            }
            _ => Ok(None),
        }
    }

    fn ty(&mut self) -> Result<Option<PreType>, Error> {
        match self.peek().as_deref() {
            Some(Tok::I32) => {
                self.next();
                Ok(Some(PreType::I32))
            }
            Some(Tok::I64) => {
                self.next();
                Ok(Some(PreType::I64))
            }
            Some(Tok::OpenParen) => {
                self.next();
                self.expect(Tok::CloseParen, "closing ')'")?;
                Ok(Some(PreType::Unit))
            }
            _ => Ok(None),
        }
    }

    fn item(&mut self) -> Result<Option<PreItem>, Error> {
        match self.peek().as_deref() {
            None => Ok(None),
            Some(Tok::Fn) => {
                // fn f(x: T, y: T): Z = x
                self.next();
                let name = self.name().ok_or(self.err("expected function name"))?;
                self.expect(Tok::OpenParen, "'('")?;
                let mut args = Vec::new();
                while self.peek().as_deref() != Some(&Tok::CloseParen) {
                    let n = self
                        .name()
                        .ok_or(self.err("expected argument name or ')'"))?;
                    self.expect(Tok::Colon, "':'")?;

                    let t = self.ty()?.ok_or(self.err("expected argument type"))?;
                    args.push((n.inner, t));
                    match self.peek().as_deref() {
                        Some(Tok::Comma) => {
                            self.next();
                        }
                        Some(Tok::CloseParen) => (),
                        _ => return Err(self.err("expected ',' or ')'")),
                    }
                }
                self.expect(Tok::CloseParen, "closing ')'")?;
                let ret_type = if let Some(&Tok::Colon) = self.peek().as_deref() {
                    self.next();
                    self.ty()?.ok_or(self.err("expected return type"))?
                } else {
                    PreType::Unit
                };

                let body = match self.peek().as_deref() {
                    Some(Tok::Equals) => {
                        self.next();
                        let t = self.term()?;
                        self.expect(Tok::Semicolon, "';' to end function body")?;
                        t
                    }
                    // let term() consume the brace
                    Some(Tok::OpenBrace) => self.term()?,
                    _ => return Err(self.err("expected '=' or '{' to start function body")),
                }
                .ok_or(self.err("expected function body"))?;

                Ok(Some(PreItem::Fn(PreFn {
                    name,
                    ret_ty: ret_type,
                    args,
                    body,
                })))
            }
            _ => Ok(None),
        }
    }

    fn stmt(&mut self) -> Result<Option<PreStatement>, Error> {
        match self.peek().as_deref() {
            Some(Tok::Fn) => Ok(self.item()?.map(PreStatement::Item)),
            Some(Tok::Let) => {
                self.next();

                let name = *self.name().ok_or(self.err("expected name"))?;
                let ty = if self.peek().as_deref() == Some(&Tok::Colon) {
                    self.next();
                    Some(self.ty()?.ok_or(self.err("expected type"))?)
                } else {
                    None
                };

                if self.next().as_deref() != Some(&Tok::Equals) {
                    return Err(self.err("expected '='"));
                }

                let x = self.term()?.ok_or(self.err("expected expression"))?;

                if self.next().as_deref() != Some(&Tok::Semicolon) {
                    return Err(self.err("expected ';'"));
                }

                Ok(Some(PreStatement::Let(name, ty, x)))
            }
            _ => Ok(self.term()?.map(PreStatement::Term)),
        }
    }

    pub fn top_level(&mut self) -> Result<Vec<PreItem>, Error> {
        let mut v = Vec::new();
        while self.peek().is_some() {
            match self.item()? {
                Some(x) => v.push(x),
                None => {
                    if let Some(x) = self.next_err.take() {
                        return Err(x);
                    } else {
                        return Err(Spanned::new(
                            Doc::start("Unexpected ")
                                .debug(*self.peek().unwrap())
                                .add(", expected statement"),
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

    pub fn finish(self) -> Bindings {
        self.bindings
    }
}
