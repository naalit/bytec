use crate::term::*;
use std::str::FromStr;

// LEXER
// This is basically Rust syntax
#[derive(Clone, Debug, PartialEq)]
enum Tok<'a> {
    // x
    Name(&'a str),
    // 3
    LitI(i64),
    // 3.5
    LitF(f64),
    // "Hello world!\n"
    LitS(String),
    // extern { ... }
    ExternBlock(&'a str),

    // fn
    Fn,
    // i32
    I32,
    // i64
    I64,
    // str
    Str,
    // let
    Let,
    // extern
    Extern,
    // pub
    Pub,
    // if
    If,
    // else
    Else,
    // bool
    Bool,

    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // >
    Gt,
    // <
    Lt,
    // ==
    Eq,
    // !=
    Neq,
    // >=
    Geq,
    // <=
    Leq,

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
    fn peek(&self) -> Option<char> {
        self.input.as_bytes().get(self.pos).copied().map(char::from)
    }
    fn peekn(&self, n: usize) -> Option<char> {
        self.input
            .as_bytes()
            .get(self.pos + n)
            .copied()
            .map(char::from)
    }
    fn nextc(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn is_ident_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn alpha(&mut self) -> Result<Spanned<Tok<'a>>, Error> {
        let start = self.pos;
        while self.peek().map_or(false, Lexer::is_ident_char) {
            self.pos += 1;
        }
        let name = &self.input[start..self.pos];
        let tok = match name {
            "fn" => Tok::Fn,
            "i32" => Tok::I32,
            "i64" => Tok::I64,
            "str" => Tok::Str,
            "let" => Tok::Let,
            "extern" => Tok::Extern,
            "pub" => Tok::Pub,
            "if" => Tok::If,
            "else" => Tok::Else,
            "bool" => Tok::Bool,
            _ => Tok::Name(name),
        };

        // Handle extern blocks: `extern { ...java code... }`
        if tok == Tok::Extern {
            let end_extern = self.pos;
            while self.peek().map_or(false, char::is_whitespace) {
                self.nextc();
            }
            if self.peek() == Some('{') {
                self.nextc();
                let start_java = self.pos;
                let mut nbrackets = 1;
                loop {
                    match self.nextc() {
                        // Allow nested {}
                        Some('{') => nbrackets += 1,
                        Some('}') => {
                            nbrackets -= 1;
                            if nbrackets == 0 {
                                let java = &self.input[start_java..self.pos - 1];
                                return Ok(Spanned::new(
                                    Tok::ExternBlock(java),
                                    Span(start, self.pos),
                                ));
                            }
                        }

                        // Any character but } will just be in the Java code
                        Some(_) => (),
                        None => {
                            return Err(Spanned::new(
                                Doc::start("expected '}'"),
                                Span(self.pos - 1, self.pos),
                            ))
                        }
                    }
                }
            } else {
                return Ok(Spanned::new(tok, Span(start, end_extern)));
            }
        }

        Ok(Spanned::new(tok, Span(start, self.pos)))
    }

    fn single<T>(&mut self, tok: Tok<'a>) -> Option<Result<Spanned<Tok<'a>>, T>> {
        self.pos += 1;
        Some(Ok(Spanned::new(tok, Span(self.pos - 1, self.pos))))
    }
    fn single_n<T>(&mut self, tok: Tok<'a>, n: usize) -> Option<Result<Spanned<Tok<'a>>, T>> {
        self.pos += n;
        Some(Ok(Spanned::new(tok, Span(self.pos - n, self.pos))))
    }

    fn lex_number(&mut self) -> Result<Spanned<Tok<'a>>, Error> {
        let start = self.pos;
        let mut buf = String::new();
        let neg = self.peek() == Some('-');
        if neg {
            buf.push(self.nextc().unwrap());
        }
        let mut base = 10;
        if self.peek() == Some('0') {
            buf.push(self.nextc().unwrap());
            match self.peek() {
                Some('x') => {
                    self.nextc();
                    base = 16;
                }
                Some('b') => {
                    self.nextc();
                    base = 2;
                }
                _ => (),
            }
        }
        let mut float = false;
        while let Some(next) = self.peek() {
            if next.is_digit(base) {
                buf.push(next);
                self.nextc();
            } else if next == '_' {
                self.nextc();
            } else if next.is_alphanumeric() {
                return Err(Spanned::new(
                    Doc::start("Invalid digit for int literal: '")
                        .add(next)
                        .add("'"),
                    Span(self.pos, self.pos + 1),
                ));
            } else if next == '.' {
                float = true;
                buf.push(next);
                self.nextc();
            } else {
                break;
            }
        }
        Ok(Spanned::new(
            if float {
                Tok::LitF(
                    f64::from_str(&buf)
                        .map_err(|e| Spanned::new(Doc::start(e), Span(start, self.pos)))?,
                )
            } else {
                Tok::LitI(
                    i64::from_str_radix(&buf, base)
                        .map_err(|e| Spanned::new(Doc::start(e), Span(start, self.pos)))?,
                )
            },
            Span(start, self.pos),
        ))
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Tok<'a>>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peek()? {
            '+' => self.single(Tok::Add),
            '-' => self.single(Tok::Sub),
            '*' => self.single(Tok::Mul),
            '/' => self.single(Tok::Div),

            '=' if self.peekn(1) == Some('=') => self.single_n(Tok::Eq, 2),
            '!' if self.peekn(1) == Some('=') => self.single_n(Tok::Neq, 2),
            '>' if self.peekn(1) == Some('=') => self.single_n(Tok::Geq, 2),
            '<' if self.peekn(1) == Some('=') => self.single_n(Tok::Leq, 2),
            '>' => self.single(Tok::Gt),
            '<' => self.single(Tok::Lt),

            '(' => self.single(Tok::OpenParen),
            ')' => self.single(Tok::CloseParen),
            '{' => self.single(Tok::OpenBrace),
            '}' => self.single(Tok::CloseBrace),
            ':' => self.single(Tok::Colon),
            ';' => self.single(Tok::Semicolon),
            '=' => self.single(Tok::Equals),
            ',' => self.single(Tok::Comma),

            '"' => {
                let start = self.pos;
                self.nextc();
                let mut buf = String::new();
                loop {
                    match self.nextc() {
                        Some('"') => {
                            break Some(Ok(Spanned::new(Tok::LitS(buf), Span(start, self.pos))))
                        }
                        Some('\\') => {
                            // Escape
                            match self.nextc() {
                                Some('\\') => {
                                    buf.push('\\');
                                }
                                Some('n') => {
                                    buf.push('\n');
                                }
                                Some('t') => {
                                    buf.push('\t');
                                }
                                Some(c) => {
                                    break Some(Err(Spanned::new(
                                        Doc::start(format!("Invalid escape '\\{}'", c)),
                                        Span(self.pos - 2, self.pos),
                                    )))
                                }
                                None => {
                                    break Some(Err(Spanned::new(
                                        Doc::start("expected closing '\"'"),
                                        Span(start, self.pos),
                                    )))
                                }
                            }
                        }
                        Some(c) => buf.push(c),
                        None => {
                            break Some(Err(Spanned::new(
                                Doc::start("expected closing '\"'"),
                                Span(start, self.pos),
                            )))
                        }
                    }
                }
            }

            x if x.is_whitespace() => {
                self.pos += 1;
                self.next()
            }
            x if x.is_alphabetic() || x == '_' => Some(self.alpha()),
            x if x.is_ascii_digit() => Some(self.lex_number()),

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
        match &self.next {
            Some(x) => Some(x.clone()),
            None => {
                if self.next_err.is_some() {
                    return None;
                }
                if let Some(x) = self.lexer.next() {
                    match x {
                        Ok(x) => {
                            self.next = Some(x.clone());
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
        Span(self.lexer.pos - 1, self.lexer.pos)
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
        let t = match self.arith()? {
            Some(t) => t,
            None => return Ok(None),
        };

        let op = match self.peek().as_deref() {
            Some(Tok::Gt) => BinOp::Gt,
            Some(Tok::Lt) => BinOp::Lt,
            Some(Tok::Eq) => BinOp::Eq,
            Some(Tok::Neq) => BinOp::Neq,
            Some(Tok::Geq) => BinOp::Geq,
            Some(Tok::Leq) => BinOp::Leq,
            _ => return Ok(Some(t)),
        };

        self.next();

        let rhs = self.arith()?.ok_or(self.err("expected expression"))?;
        let span = Span(t.span.0, rhs.span.1);

        Ok(Some(Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span))))
    }

    fn arith(&mut self) -> Result<Option<SPre>, Error> {
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
            Some(Tok::LitI(i)) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(
                    Pre::Lit(Literal::Int(*i), None),
                    self.span(),
                ))))
            }
            Some(Tok::LitS(s)) => {
                let q = self.next().unwrap();
                Ok(Some(Box::new(Spanned::new(
                    Pre::Lit(Literal::Str(self.bindings.raw(s)), None),
                    q.span,
                ))))
            }
            Some(Tok::If) => {
                let start = self.lexer.pos;
                self.next();
                let cond = self.term()?.ok_or(self.err("expected if condition"))?;

                // Make sure there's a block next, but don't consume the {
                // term() will do that itself
                if self.peek().as_deref() != Some(&Tok::OpenBrace) {
                    return Err(self.err("expected '{' after if condition"));
                }
                let a = self.term()?.unwrap();

                let b = if self.peek().as_deref() == Some(&Tok::Else) {
                    self.next();
                    if self.peek().as_deref() != Some(&Tok::OpenBrace) {
                        return Err(self.err("expected '{' after 'else'"));
                    }
                    Some(self.term()?.unwrap())
                } else {
                    None
                };

                Ok(Some(Box::new(Spanned::new(
                    Pre::If(cond, a, b),
                    Span(start, self.lexer.pos),
                ))))
            }
            // variable or function call
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
            Some(Tok::Bool) => {
                self.next();
                Ok(Some(PreType::Bool))
            }
            Some(Tok::Str) => {
                self.next();
                Ok(Some(PreType::Str))
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
            Some(Tok::ExternBlock(s)) => {
                self.next();
                Ok(Some(PreItem::InlineJava(self.bindings.raw(*s))))
            }
            Some(Tok::Extern | Tok::Fn) => {
                // fn f(x: T, y: T): Z = x
                let (public, ext) = match &*self.next().unwrap() {
                    Tok::Fn => match self.peek().as_deref() {
                        Some(Tok::Pub) => {
                            self.next();
                            (true, false)
                        }
                        _ => (false, false),
                    },
                    Tok::Extern => match self.next().as_deref() {
                        Some(Tok::Fn) => (false, true),
                        Some(Tok::LitS(s)) => {
                            self.expect(Tok::Semicolon, "';'")?;
                            return Ok(Some(PreItem::InlineJava(self.bindings.raw(s))));
                        }
                        _ => return Err(self.err("expected 'fn' or inline Java string")),
                    },
                    _ => unreachable!(),
                };
                let name = self.name().ok_or(self.err("expected function name"))?;
                self.expect(Tok::OpenParen, "'('")?;
                let mut args = Vec::new();
                while self.peek().as_deref() != Some(&Tok::CloseParen) {
                    let public = if self.peek().as_deref() == Some(&Tok::Pub) {
                        self.next();
                        true
                    } else {
                        false
                    };

                    let n = self
                        .name()
                        .ok_or(self.err("expected argument name or ')'"))?;
                    self.expect(Tok::Colon, "':'")?;

                    let t = self.ty()?.ok_or(self.err("expected argument type"))?;
                    args.push((n.inner, t, public));
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

                if ext {
                    self.expect(Tok::Equals, "'='")?;
                    let mapping = match self.next().as_deref() {
                        Some(Tok::LitS(m)) => self.bindings.raw(m),
                        _ => return Err(self.err("expected Java function name as string literal")),
                    };
                    self.expect(Tok::Semicolon, "';'")?;

                    Ok(Some(PreItem::ExternFn(PreEFn {
                        name,
                        ret_ty: ret_type,
                        args,
                        mapping,
                    })))
                } else {
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
                        public,
                    })))
                }
            }
            _ => Ok(None),
        }
    }

    fn stmt(&mut self) -> Result<Option<PreStatement>, Error> {
        match self.peek().as_deref() {
            Some(Tok::Fn | Tok::Extern | Tok::ExternBlock(_)) => {
                Ok(self.item()?.map(PreStatement::Item))
            }
            Some(Tok::Let) => {
                self.next();

                let public = if self.peek().as_deref() == Some(&Tok::Pub) {
                    self.next();
                    true
                } else {
                    false
                };

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

                let value = self.term()?.ok_or(self.err("expected expression"))?;

                if self.next().as_deref() != Some(&Tok::Semicolon) {
                    return Err(self.err("expected ';'"));
                }

                Ok(Some(PreStatement::Let {
                    name,
                    ty,
                    value,
                    public,
                }))
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
                                .debug(self.peek().unwrap().inner)
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
