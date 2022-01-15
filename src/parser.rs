use crate::term::*;
use std::{collections::HashMap, str::FromStr};

// LEXER
// This is basically Rust syntax
#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    // x
    Name(RawSym),
    // 3
    LitI(i64),
    // 3.5
    LitF(f64),
    // "Hello world!\n"
    LitS(String),
    // extern { ... }
    ExternBlock(RawSym),

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
    // class
    Class,
    // match
    Match,
    // enum
    Enum,
    // true
    True,
    // false
    False,
    // loop
    Loop,
    // while
    While,
    // break
    Break,
    // continue
    Continue,
    // return
    Return,
    // for
    For,
    // in
    In,
    // use
    Use,
    // constructor
    Constructor,
    // throws
    Throws,
    // null
    Null,
    // self
    Selph,
    // #define
    Define,
    // #ifdef
    IfDef,
    // unroll
    Unroll,
    // inline
    Inline,

    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // %
    Mod,
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
    // =>
    WideArrow,
    // ::
    DoubleColon,
    // &
    BitAnd,
    // |
    BitOr,
    // ^
    BitXor,
    // >>
    BitShr,
    // <<
    BitShl,
    // &&
    And,
    // ||
    Or,
    // !
    Not,

    // (
    OpenParen,
    // )
    CloseParen,
    // {
    OpenBrace,
    // }
    CloseBrace,
    // [
    OpenBracket,
    // ]
    CloseBracket,
    // :
    Colon,
    // =
    Equals,
    // ;
    Semicolon,
    // ,
    Comma,
    // .
    Dot,
    // ..
    DotDot,
}
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    bindings: &'a mut Bindings,
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

    fn alpha(&mut self) -> Result<Spanned<Tok>, Error> {
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
            "class" => Tok::Class,
            "match" => Tok::Match,
            "enum" => Tok::Enum,
            "true" => Tok::True,
            "false" => Tok::False,
            "loop" => Tok::Loop,
            "while" => Tok::While,
            "break" => Tok::Break,
            "continue" => Tok::Continue,
            "return" => Tok::Return,
            "for" => Tok::For,
            "in" => Tok::In,
            "use" => Tok::Use,
            "constructor" => Tok::Constructor,
            "throws" => Tok::Throws,
            "null" => Tok::Null,
            "self" => Tok::Selph,
            "define" => Tok::Define,
            "ifdef" => Tok::IfDef,
            "unroll" => Tok::Unroll,
            "inline" => Tok::Inline,
            _ => Tok::Name(self.bindings.raw(name)),
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
                                    Tok::ExternBlock(self.bindings.raw(java)),
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

    fn single<T>(&mut self, tok: Tok) -> Option<Result<Spanned<Tok>, T>> {
        self.pos += 1;
        Some(Ok(Spanned::new(tok, Span(self.pos - 1, self.pos))))
    }
    fn single_n<T>(&mut self, tok: Tok, n: usize) -> Option<Result<Spanned<Tok>, T>> {
        self.pos += n;
        Some(Ok(Spanned::new(tok, Span(self.pos - n, self.pos))))
    }

    fn lex_number(&mut self) -> Result<Spanned<Tok>, Error> {
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
            } else if next == '.' && !float && self.peekn(1) != Some('.') {
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
    type Item = Result<Spanned<Tok>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peek()? {
            '/' if self.peekn(1) == Some('/') => {
                self.pos += 2;
                while self.nextc().map_or(false, |x| x != '\n') {}
                self.next()
            }

            '-' if self.peekn(1).map_or(false, |x| x.is_ascii_digit()) => Some(self.lex_number()),

            '+' => self.single(Tok::Add),
            '-' => self.single(Tok::Sub),
            '*' => self.single(Tok::Mul),
            '/' => self.single(Tok::Div),
            '%' => self.single(Tok::Mod),

            '=' if self.peekn(1) == Some('=') => self.single_n(Tok::Eq, 2),
            '!' if self.peekn(1) == Some('=') => self.single_n(Tok::Neq, 2),
            '>' if self.peekn(1) == Some('=') => self.single_n(Tok::Geq, 2),
            '<' if self.peekn(1) == Some('=') => self.single_n(Tok::Leq, 2),
            '=' if self.peekn(1) == Some('>') => self.single_n(Tok::WideArrow, 2),
            ':' if self.peekn(1) == Some(':') => self.single_n(Tok::DoubleColon, 2),
            '.' if self.peekn(1) == Some('.') => self.single_n(Tok::DotDot, 2),
            '&' if self.peekn(1) == Some('&') => self.single_n(Tok::And, 2),
            '|' if self.peekn(1) == Some('|') => self.single_n(Tok::Or, 2),
            '>' if self.peekn(1) == Some('>') => self.single_n(Tok::BitShr, 2),
            '<' if self.peekn(1) == Some('<') => self.single_n(Tok::BitShl, 2),

            '>' => self.single(Tok::Gt),
            '<' => self.single(Tok::Lt),
            '&' => self.single(Tok::BitAnd),
            '|' => self.single(Tok::BitOr),
            '^' => self.single(Tok::BitXor),

            '(' => self.single(Tok::OpenParen),
            ')' => self.single(Tok::CloseParen),
            '{' => self.single(Tok::OpenBrace),
            '}' => self.single(Tok::CloseBrace),
            '[' => self.single(Tok::OpenBracket),
            ']' => self.single(Tok::CloseBracket),
            ':' => self.single(Tok::Colon),
            ';' => self.single(Tok::Semicolon),
            '=' => self.single(Tok::Equals),
            ',' => self.single(Tok::Comma),
            '.' => self.single(Tok::Dot),
            '!' => self.single(Tok::Not),

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
                                Some('"') => {
                                    buf.push('"');
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

pub fn lex_one(input: &str, bindings: &mut Bindings) -> Option<Tok> {
    let mut lexer = Lexer {
        input,
        bindings,
        pos: 0,
    };
    Some(lexer.next()?.ok()?.inner)
}

enum IfDef {
    Yes,
    No(bool),
}
impl IfDef {
    fn resolve(self, p: &mut Parser) -> bool {
        match self {
            IfDef::Yes => true,
            IfDef::No(set_isreal) => {
                if set_isreal {
                    p.is_real = true;
                }
                false
            }
        }
    }
}

// PARSER
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    next: Option<Spanned<Tok>>,
    next_err: Option<Error>,
    is_real: bool,
    in_ifdef: bool,
    defs: &'a mut HashMap<RawSym, Option<Tok>>,
}
impl<'a> Parser<'a> {
    pub fn new(
        input: &'a str,
        bindings: &'a mut Bindings,
        defs: &'a mut HashMap<RawSym, Option<Tok>>,
    ) -> Self {
        Parser {
            lexer: Lexer {
                input,
                bindings,
                pos: 0,
            },
            next: None,
            next_err: None,
            in_ifdef: false,
            is_real: true,
            defs,
        }
    }

    fn peek(&mut self) -> Option<Spanned<Tok>> {
        match &self.next {
            Some(x) => Some(x.clone()),
            None => {
                if self.next_err.is_some() {
                    return None;
                }
                if let Some(x) = self.lexer.next() {
                    match x {
                        Ok(Spanned {
                            inner: Tok::Name(r),
                            span,
                        }) if !self.in_ifdef && self.defs.contains_key(&r) => {
                            let x = self.defs.get(&r).unwrap();
                            match x {
                                Some(t) => {
                                    let t = Spanned::new(t.clone(), span);
                                    self.next = Some(t.clone());
                                    Some(t)
                                }
                                None => self.peek(),
                            }
                        }
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

    fn ifdef(&mut self) -> Result<IfDef, Error> {
        if self.peek().as_deref() == Some(&Tok::IfDef) {
            self.next();
            self.in_ifdef = true;
            let wanted = if self.peek().as_deref() == Some(&Tok::Not) {
                self.next();
                false
            } else {
                true
            };
            let ident = self
                .ident()
                .ok_or(self.err("expected identifier after ifdef"))?;
            self.in_ifdef = false;
            let defined = self.defs.contains_key(&*ident);
            if defined == wanted {
                Ok(IfDef::Yes)
            } else {
                if self.is_real {
                    self.is_real = false;
                    Ok(IfDef::No(true))
                } else {
                    Ok(IfDef::No(false))
                }
            }
        } else {
            Ok(IfDef::Yes)
        }
    }

    fn defines(&mut self) -> Result<(), Error> {
        while self.peek().as_deref() == Some(&Tok::Define) {
            self.next();
            let name = self
                .ident()
                .ok_or(self.err("expected identifier after define"))?;
            if self.peek().as_deref() == Some(&Tok::Equals) {
                self.next();
                let val = self.next().ok_or(self.err("expected token after '='"))?;
                self.expect(Tok::Semicolon, "';'")?;
                if self.is_real {
                    self.defs.insert(*name, Some(val.inner.clone()));
                }
            } else {
                self.expect(Tok::Semicolon, "'=' or ';'")?;
                if self.is_real {
                    self.defs.insert(*name, None);
                }
            }
        }
        Ok(())
    }

    fn next(&mut self) -> Option<Spanned<Tok>> {
        let next = self.peek();
        self.next = None;
        next
    }

    fn err(&mut self, err: &(impl std::fmt::Display + ?Sized)) -> Error {
        Spanned::new(Doc::start(err), self.span())
    }

    fn span(&mut self) -> Span {
        self.peek()
            .map(|x| x.span)
            .unwrap_or(Span(self.lexer.pos - 1, self.lexer.pos))
    }

    fn expect(&mut self, tok: Tok, msg: &str) -> Result<(), Error> {
        if self.peek().as_deref() == Some(&tok) {
            self.next();
            Ok(())
        } else {
            Err(self.err(&format!("expected {}", msg)))
        }
    }

    fn ident(&mut self) -> Option<Spanned<RawSym>> {
        match *self.peek()? {
            Tok::Name(x) => {
                let t = self.next()?;
                Some(Spanned::new(x, t.span))
            }
            _ => None,
        }
    }

    fn path(&mut self) -> Option<RawPath> {
        let mut name = self.ident()?;
        let mut v = Vec::new();
        while self.peek().as_deref() == Some(&Tok::DoubleColon) {
            self.next();
            v.push(name);
            name = self.ident().unwrap();
        }
        Some(RawPath(v, name))
    }

    fn term(&mut self) -> Result<Option<SPre>, Error> {
        let t = match self.logic()? {
            Some(t) => t,
            None => return Ok(None),
        };

        if self.peek().as_deref() == Some(&Tok::Equals) {
            self.next();
            let rhs = self.logic()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            return Ok(Some(Box::new(Spanned::new(Pre::Set(t, None, rhs), span))));
        }

        Ok(Some(t))
    }

    fn logic(&mut self) -> Result<Option<SPre>, Error> {
        let mut t = match self.comparison()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            let op = match self.peek().as_deref() {
                Some(Tok::And) => BinOp::And,
                Some(Tok::Or) => BinOp::Or,
                _ => break,
            };
            self.next();

            if self.peek().as_deref() == Some(&Tok::Equals) {
                self.next();
                let rhs = self.term()?.ok_or(self.err("expected expression"))?;
                let span = Span(t.span.0, rhs.span.1);
                t = Box::new(Spanned::new(Pre::Set(t, Some(op), rhs), span));
                break;
            }

            let rhs = self.comparison()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
    }

    fn comparison(&mut self) -> Result<Option<SPre>, Error> {
        let t = match self.bitwise()? {
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

        let rhs = self.bitwise()?.ok_or(self.err("expected expression"))?;
        let span = Span(t.span.0, rhs.span.1);

        Ok(Some(Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span))))
    }

    fn bitwise(&mut self) -> Result<Option<SPre>, Error> {
        let mut t = match self.arith()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            let op = match self.peek().as_deref() {
                Some(Tok::BitAnd) => BinOp::BitAnd,
                Some(Tok::BitOr) => BinOp::BitOr,
                Some(Tok::BitXor) => BinOp::BitXor,
                Some(Tok::BitShr) => BinOp::BitShr,
                Some(Tok::BitShl) => BinOp::BitShl,
                _ => break,
            };
            self.next();

            if self.peek().as_deref() == Some(&Tok::Equals) {
                self.next();
                let rhs = self.term()?.ok_or(self.err("expected expression"))?;
                let span = Span(t.span.0, rhs.span.1);
                t = Box::new(Spanned::new(Pre::Set(t, Some(op), rhs), span));
                break;
            }

            let rhs = self.arith()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
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

            if self.peek().as_deref() == Some(&Tok::Equals) {
                self.next();
                let rhs = self.term()?.ok_or(self.err("expected expression"))?;
                let span = Span(t.span.0, rhs.span.1);
                t = Box::new(Spanned::new(Pre::Set(t, Some(op), rhs), span));
                break;
            }

            let rhs = self.factor()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
    }

    fn factor(&mut self) -> Result<Option<SPre>, Error> {
        let mut t = match self.method()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            let op = match self.peek().as_deref() {
                Some(Tok::Mul) => BinOp::Mul,
                Some(Tok::Div) => BinOp::Div,
                Some(Tok::Mod) => BinOp::Mod,
                _ => break,
            };
            self.next();

            if self.peek().as_deref() == Some(&Tok::Equals) {
                self.next();
                let rhs = self.term()?.ok_or(self.err("expected expression"))?;
                let span = Span(t.span.0, rhs.span.1);
                t = Box::new(Spanned::new(Pre::Set(t, Some(op), rhs), span));
                break;
            }

            let rhs = self.method()?.ok_or(self.err("expected expression"))?;
            let span = Span(t.span.0, rhs.span.1);
            t = Box::new(Spanned::new(Pre::BinOp(op, t, rhs), span));
        }

        Ok(Some(t))
    }

    fn method(&mut self) -> Result<Option<SPre>, Error> {
        let not = if self.peek().as_deref() == Some(&Tok::Not) {
            let start = self.lexer.pos;
            self.next();
            Some(start)
        } else {
            None
        };

        let mut t = match self.atom()? {
            Some(t) => t,
            None => return Ok(None),
        };

        loop {
            match self.peek().as_deref() {
                Some(Tok::Dot) => {
                    self.next();

                    if let Some(Tok::LitI(i)) = self.peek().as_deref() {
                        self.next();
                        let span = Span(t.span.0, self.lexer.pos);
                        t = Box::new(Spanned::new(Pre::TupleIdx(t, *i as usize), span));
                    } else {
                        let name = self.ident().ok_or(self.err("expected method name"))?;
                        if self.peek().as_deref() == Some(&Tok::OpenParen) {
                            let args = self.call_args()?;
                            let span = Span(t.span.0, self.lexer.pos);
                            t = Box::new(Spanned::new(Pre::Method(t, name, args), span));
                        } else {
                            let span = Span(t.span.0, self.lexer.pos);
                            t = Box::new(Spanned::new(Pre::Member(t, name), span));
                        }
                    }
                }
                Some(Tok::OpenBracket) => {
                    self.next();
                    let inline = if self.peek().as_deref() == Some(&Tok::Inline) {
                        self.next();
                        true
                    } else {
                        false
                    };
                    let idx = self.term()?.ok_or(self.err("expected expression"))?;
                    self.expect(Tok::CloseBracket, "closing ']'")?;
                    let span = Span(t.span.0, self.lexer.pos);
                    t = Box::new(Spanned::new(Pre::ArrayIdx(t, idx, inline), span));
                }
                _ => break,
            }
        }

        if let Some(start) = not {
            Ok(Some(Box::new(Spanned::new(
                Pre::Not(t),
                Span(start, self.lexer.pos),
            ))))
        } else {
            Ok(Some(t))
        }
    }

    fn call_args(&mut self) -> Result<Vec<SPre>, Error> {
        self.expect(Tok::OpenParen, "'('")?;
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
        match self.next().as_deref() {
            Some(Tok::CloseParen) => Ok(args),
            None => return Err(self.err("unclosed argument list, expected ')'")),
            _ => unreachable!(),
        }
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
                    Pre::Lit(Literal::Str(self.lexer.bindings.raw(s)), None),
                    q.span,
                ))))
            }
            Some(Tok::True) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(
                    Pre::Lit(Literal::Bool(true), None),
                    self.span(),
                ))))
            }
            Some(Tok::False) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(
                    Pre::Lit(Literal::Bool(false), None),
                    self.span(),
                ))))
            }
            Some(Tok::Null) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(Pre::Null, self.span()))))
            }
            Some(Tok::Selph) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(Pre::Selph, self.span()))))
            }
            Some(Tok::Break) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(Pre::Break, self.span()))))
            }
            Some(Tok::Continue) => {
                self.next();
                Ok(Some(Box::new(Spanned::new(Pre::Continue, self.span()))))
            }
            Some(Tok::Return) => {
                self.next();
                let x = self.term()?;
                Ok(Some(Box::new(Spanned::new(Pre::Return(x), self.span()))))
            }
            Some(Tok::Sub) => {
                let zero = Box::new(Spanned::new(Pre::Lit(Literal::Int(0), None), self.span()));
                self.next();
                let x = self.atom()?.ok_or(self.err("expected expression"))?;
                Ok(Some(Box::new(Spanned::new(
                    Pre::BinOp(BinOp::Sub, zero, x),
                    self.span(),
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
                    if !matches!(self.peek().as_deref(), Some(Tok::If | Tok::OpenBrace)) {
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
            Some(Tok::Match) => {
                let start = self.lexer.pos;
                self.next();
                let scrutinee = self
                    .term()?
                    .ok_or(self.err("expected expression to match on"))?;

                self.expect(Tok::OpenBrace, "'{'")?;

                let mut branches = Vec::new();
                loop {
                    let needs_semicolon = match self.peek().as_deref() {
                        Some(Tok::Name(_)) => {
                            let name = self.ident().unwrap();

                            let mut captures = Vec::new();
                            if self.peek().as_deref() == Some(&Tok::OpenParen) {
                                self.next();
                                loop {
                                    if self.peek().as_deref() == Some(&Tok::CloseParen) {
                                        self.next();
                                        break;
                                    }

                                    let mut public = false;
                                    if self.peek().as_deref() == Some(&Tok::Pub) {
                                        public = true;
                                    }

                                    let name = self.ident().ok_or(self.err("expected name"))?;
                                    captures.push((name, public));
                                    if self.peek().as_deref() == Some(&Tok::Comma) {
                                        self.next();
                                    } else {
                                        self.expect(Tok::CloseParen, "closing ')'")?;
                                        break;
                                    }
                                }
                            }

                            self.expect(Tok::WideArrow, "'=>'")?;
                            let term = self.term()?.ok_or(self.err("expected expression"))?;
                            let n = term.needs_semicolon();
                            branches.push((Spanned::new(Some(*name), name.span), captures, term));
                            n
                        }
                        Some(Tok::Else) => {
                            let espan = self.span();
                            self.next();
                            self.expect(Tok::WideArrow, "'=>'")?;
                            let term = self.term()?.ok_or(self.err("expected expression"))?;
                            let n = term.needs_semicolon();
                            branches.push((Spanned::new(None, espan), Vec::new(), term));
                            n
                        }
                        Some(Tok::CloseBrace) => {
                            self.next();
                            break;
                        }
                        _ => return Err(self.err("expected match branch or '}'")),
                    };
                    match self.peek().as_deref() {
                        Some(Tok::Comma) => {
                            self.next();
                        }
                        Some(Tok::CloseBrace) => {
                            self.next();
                            break;
                        }
                        _ if !needs_semicolon => (),
                        _ => return Err(self.err("expected ',' or '}'")),
                    }
                }

                Ok(Some(Box::new(Spanned::new(
                    Pre::Match(scrutinee, branches),
                    Span(start, self.lexer.pos),
                ))))
            }
            // variable or function or method call
            Some(Tok::Name(_)) => {
                let name = self.path().unwrap();
                let var = Box::new(Spanned::new(Pre::Var(name.clone()), name.span()));
                match self.peek().as_deref() {
                    Some(Tok::OpenParen) => {
                        let args = self.call_args()?;
                        Ok(Some(Box::new(Spanned::new(
                            Pre::Call(name, args),
                            Span(var.span.0, self.lexer.pos),
                        ))))
                    }
                    _ => Ok(Some(var)),
                }
            }
            Some(Tok::OpenParen) => {
                let start = self.lexer.pos;
                self.next();

                let mut v = Vec::new();
                loop {
                    if self.peek().as_deref() == Some(&Tok::CloseParen) {
                        self.next();
                        break;
                    }

                    v.push(self.term()?.ok_or(self.err("expected term"))?);
                    if self.peek().as_deref() == Some(&Tok::Comma) {
                        self.next();
                    } else {
                        self.expect(Tok::CloseParen, "closing ')'")?;
                        break;
                    }
                }

                if v.len() == 1 {
                    Ok(Some(v.pop().unwrap()))
                } else {
                    Ok(Some(Box::new(Spanned::new(
                        Pre::Tuple(v),
                        Span(start, self.lexer.pos),
                    ))))
                }
            }
            Some(Tok::OpenBracket) => {
                let start = self.lexer.pos;
                self.next();

                let mut v = Vec::new();
                loop {
                    if self.peek().as_deref() == Some(&Tok::CloseBracket) {
                        self.next();
                        break;
                    }

                    v.push(self.term()?.ok_or(self.err("expected term"))?);
                    if self.peek().as_deref() == Some(&Tok::Comma) {
                        self.next();
                    } else {
                        self.expect(Tok::CloseBracket, "closing ']'")?;
                        break;
                    }
                }

                Ok(Some(Box::new(Spanned::new(
                    Pre::Array(v),
                    Span(start, self.lexer.pos),
                ))))
            }
            Some(Tok::OpenBrace) => {
                // block
                let start = self.lexer.pos;
                self.next();
                let mut block = Vec::new();
                if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                    self.next();
                    return Ok(Some(Box::new(Spanned::new(
                        Pre::Block(Vec::new(), None),
                        Span(start, self.lexer.pos),
                    ))));
                }
                loop {
                    let x = self.stmt()?;
                    if x.is_none() && self.peek().as_deref() == Some(&Tok::CloseBrace) {
                        self.next();
                        return Ok(Some(Box::new(Spanned::new(
                            Pre::Block(block, None),
                            Span(start, self.lexer.pos),
                        ))));
                    } else if x.is_none() {
                        return Err(self.err("expected statement"));
                    }
                    match (x.unwrap(), self.peek().as_deref()) {
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
                        (PreStatement::Term(x), _) if x.needs_semicolon() => {
                            return Err(self.err("expected ';' after expression statement"))
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
            Some(Tok::OpenBracket) => {
                self.next();
                let inner = self.ty()?.ok_or(self.err("expected type"))?;
                let i = if self.peek().as_deref() == Some(&Tok::Semicolon) {
                    self.next();
                    if let Some(Tok::LitI(i)) = self.peek().as_deref() {
                        self.next();
                        Some(*i as usize)
                    } else {
                        return Err(self.err("expected number after ';'"));
                    }
                } else {
                    None
                };
                self.expect(Tok::CloseBracket, "closing ']'")?;
                Ok(Some(if let Some(i) = i {
                    PreType::SArray(Box::new(inner), i)
                } else {
                    PreType::Array(Box::new(inner))
                }))
            }
            Some(Tok::OpenParen) => {
                self.next();

                let mut v = Vec::new();
                loop {
                    if self.peek().as_deref() == Some(&Tok::CloseParen) {
                        self.next();
                        break;
                    }

                    v.push(self.ty()?.ok_or(self.err("expected type"))?);
                    if self.peek().as_deref() == Some(&Tok::Comma) {
                        self.next();
                    } else {
                        self.expect(Tok::CloseParen, "closing ')'")?;
                        break;
                    }
                }

                if v.len() == 1 {
                    Ok(Some(v.pop().unwrap()))
                } else {
                    Ok(Some(PreType::Tuple(v)))
                }
            }
            Some(Tok::Name(_)) => {
                let path = self.path().unwrap();
                Ok(Some(PreType::Class(path)))
            }
            _ => Ok(None),
        }
    }

    /// Parses the part of a function after the `fn` but before the `=` or `{`
    /// For example, `add(pub x: i32, y: i32): i32`
    fn prototype(
        &mut self,
    ) -> Result<
        (
            Spanned<RawSym>,
            Vec<(Spanned<RawSym>, PreType, bool)>,
            PreType,
        ),
        Error,
    > {
        let name = self.ident().ok_or(self.err("expected function name"))?;
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
                .ident()
                .ok_or(self.err("expected argument name or ')'"))?;
            self.expect(Tok::Colon, "':'")?;

            let t = self.ty()?.ok_or(self.err("expected argument type"))?;
            args.push((n, t, public));
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
            PreType::Tuple(Vec::new())
        };
        Ok((name, args, ret_type))
    }

    /// Parses an enum declaration, starting right after the `enum` keyword
    fn enum_dec(&mut self, ext: bool) -> Result<PreItem, Error> {
        let name = self.path().ok_or(self.err("expected enum name"))?;
        self.expect(Tok::OpenBrace, "'{'")?;

        let mut v = Vec::new();
        loop {
            if let Some(name) = self.ident() {
                let mut args = Vec::new();
                if self.peek().as_deref() == Some(&Tok::OpenParen) {
                    self.next();
                    loop {
                        if self.peek().as_deref() == Some(&Tok::CloseParen) {
                            self.next();
                            break;
                        }

                        args.push(self.ty()?.ok_or(self.err("expected type"))?);
                        if self.peek().as_deref() == Some(&Tok::Comma) {
                            self.next();
                        } else {
                            self.expect(Tok::CloseParen, "closing ')'")?;
                            break;
                        }
                    }
                }
                v.push((*name, args));
                if self.peek().as_deref() == Some(&Tok::Comma) {
                    self.next();
                    continue;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        let mut methods = Vec::new();
        let mut members = Vec::new();
        if self.peek().as_deref() == Some(&Tok::Semicolon) {
            self.next();
            let (methods2, members2, cons) = self.class_members(ext)?;
            if cons.is_some() {
                return Err(self.err("enum cannot have a constructor"));
            }
            methods = methods2;
            members = members2;
        }
        self.expect(Tok::CloseBrace, "closing '}'")?;

        Ok(PreItem::Class {
            path: name,
            methods,
            members,
            variants: Some(v),
            ext,
            constructor: None,
        })
    }

    fn class_members(
        &mut self,
        ext: bool,
    ) -> Result<
        (
            Vec<PreFnEither>,
            Vec<(Spanned<RawSym>, bool, PreType, Option<SPre>)>,
            Option<Vec<PreType>>,
        ),
        Error,
    > {
        let mut methods = Vec::new();
        let mut members = Vec::new();
        let mut constructor = None;

        while *self.peek().ok_or(self.err("expected closing '}'"))? != Tok::CloseBrace {
            let ifdef = self.ifdef()?;
            match self.peek().as_deref() {
                Some(Tok::Constructor) => {
                    if constructor.is_some() {
                        return Err(self.err("duplicate constructor declaration"));
                    }
                    self.next();

                    self.expect(Tok::OpenParen, "'('")?;
                    let mut args = Vec::new();
                    while self.peek().as_deref() != Some(&Tok::CloseParen) {
                        let _name = self
                            .ident()
                            .ok_or(self.err("expected argument name or ')'"))?;
                        self.expect(Tok::Colon, "':'")?;

                        let t = self.ty()?.ok_or(self.err("expected argument type"))?;
                        args.push(t);
                        match self.peek().as_deref() {
                            Some(Tok::Comma) => {
                                self.next();
                            }
                            Some(Tok::CloseParen) => (),
                            _ => return Err(self.err("expected ',' or ')'")),
                        }
                    }
                    self.expect(Tok::CloseParen, "closing ')'")?;
                    self.expect(Tok::Semicolon, "';'")?;
                    if ifdef.resolve(self) {
                        constructor = Some(args);
                    }
                }
                Some(Tok::Let) => {
                    self.next();
                    let public = ext
                        || if self.peek().as_deref() == Some(&Tok::Pub) {
                            self.next();
                            true
                        } else {
                            false
                        };
                    let name = self.ident().ok_or(self.err("expected name"))?;
                    self.expect(Tok::Colon, "':'")?;
                    let ty = self.ty()?.ok_or(self.err("expected type"))?;

                    let mut body = None;
                    if !ext && self.peek().as_deref() == Some(&Tok::Equals) {
                        self.next();
                        body = Some(self.term()?.ok_or(self.err("expected expression"))?);
                    }

                    self.expect(Tok::Semicolon, "';'")?;
                    if ifdef.resolve(self) {
                        members.push((name, public, ty, body));
                    }
                }
                Some(Tok::Fn) => {
                    self.next();
                    let (name, args, ret_ty) = self.prototype()?;
                    if ext {
                        let mapping = if *self.peek().ok_or(self.err("expected ';'"))?
                            == Tok::Equals
                        {
                            self.next();
                            if let Some(Tok::LitS(s)) = self.peek().as_deref() {
                                self.next();
                                self.lexer.bindings.raw(s)
                            } else {
                                return Err(self.err("expected Java method name as string literal"));
                            }
                        } else {
                            *name
                        };
                        self.expect(Tok::Semicolon, "';'")?;

                        let f = PreEFn {
                            name,
                            ret_ty,
                            args,
                            mapping,
                        };
                        if ifdef.resolve(self) {
                            methods.push(PreFnEither::Extern(f));
                        }
                    } else {
                        let mut throws = Vec::new();
                        if self.peek().as_deref() == Some(&Tok::Throws) {
                            self.next();
                            while let Some(n) = self.ident() {
                                throws.push(*n);
                            }
                        }

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

                        let f = PreFn {
                            name,
                            ret_ty,
                            args,
                            public: false,
                            body,
                            throws,
                            inline: false,
                        };
                        if ifdef.resolve(self) {
                            methods.push(PreFnEither::Local(f));
                        }
                    }
                }
                _ => return Err(self.err("expected item or closing '}'")),
            }
        }
        Ok((methods, members, constructor))
    }

    fn class(&mut self, ext: bool) -> Result<Option<PreItem>, Error> {
        let path = self.path().ok_or(self.err("expected class name"))?;
        match self.peek().as_deref() {
            Some(Tok::Semicolon) => {
                self.next();
                Ok(Some(PreItem::Class {
                    ext,
                    path,
                    variants: None,
                    methods: Vec::new(),
                    members: Vec::new(),
                    constructor: None,
                }))
            }
            Some(Tok::OpenBrace) => {
                self.next();
                let (methods, members, constructor) = self.class_members(ext)?;
                self.expect(Tok::CloseBrace, "'}'")?;

                Ok(Some(PreItem::Class {
                    path,
                    methods,
                    members,
                    constructor,
                    ext,
                    variants: None,
                }))
            }
            _ => return Err(self.err("expected ';' or '{'")),
        }
    }

    fn item(&mut self) -> Result<Option<PreItem>, Error> {
        self.defines()?;
        let ifdef = self.ifdef()?;
        let i = match self.peek().as_deref() {
            None => Ok(None),
            Some(Tok::ExternBlock(s)) => {
                self.next();
                Ok(Some(PreItem::InlineJava(*s)))
            }
            Some(Tok::Let) => {
                self.next();

                let public = if self.peek().as_deref() == Some(&Tok::Pub) {
                    self.next();
                    true
                } else {
                    false
                };

                let name = self.ident().ok_or(self.err("expected name"))?;
                let ty = if self.peek().as_deref() == Some(&Tok::Colon) {
                    self.next();
                    Some(self.ty()?.ok_or(self.err("expected type"))?)
                } else {
                    None
                };

                let mut value = None;
                if self.peek().as_deref() == Some(&Tok::Equals) {
                    self.next();
                    value = Some(self.term()?.ok_or(self.err("expected expression"))?);
                }

                self.expect(Tok::Semicolon, "';'")?;

                Ok(Some(PreItem::Let(name, ty, value, public)))
            }
            Some(Tok::Class) => {
                self.next();
                self.class(false)
            }
            Some(Tok::Extern | Tok::Fn | Tok::Enum) => {
                // fn f(x: T, y: T): Z = x
                let (public, ext) = match &*self.next().unwrap() {
                    Tok::Fn => match self.peek().as_deref() {
                        Some(Tok::Pub) => {
                            self.next();
                            (true, false)
                        }
                        _ => (false, false),
                    },
                    Tok::Enum => return self.enum_dec(false).map(Some),
                    Tok::Extern => match self.next().as_deref() {
                        Some(Tok::Fn) => (false, true),
                        Some(Tok::Enum) => return self.enum_dec(true).map(Some),
                        Some(Tok::LitS(s)) => {
                            self.expect(Tok::Semicolon, "';'")?;
                            return Ok(Some(PreItem::InlineJava(self.lexer.bindings.raw(s))));
                        }
                        Some(Tok::Class) => return self.class(true),
                        _ => return Err(self.err("expected 'fn', 'enum', or inline Java string")),
                    },
                    _ => unreachable!(),
                };
                let inline = if self.peek().as_deref() == Some(&Tok::Inline) {
                    self.next();
                    true
                } else {
                    false
                };
                let (name, args, ret_type) = self.prototype()?;

                if ext {
                    self.expect(Tok::Equals, "'='")?;
                    let mapping = match self.peek().as_deref() {
                        Some(Tok::LitS(m)) => self.lexer.bindings.raw(m),
                        _ => return Err(self.err("expected Java function name as string literal")),
                    };
                    self.next();
                    self.expect(Tok::Semicolon, "';'")?;

                    Ok(Some(PreItem::ExternFn(PreEFn {
                        name,
                        ret_ty: ret_type,
                        args,
                        mapping,
                    })))
                } else {
                    let mut throws = Vec::new();
                    if self.peek().as_deref() == Some(&Tok::Throws) {
                        self.next();
                        while let Some(n) = self.ident() {
                            throws.push(*n);
                        }
                    }

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
                        throws,
                        inline,
                    })))
                }
            }
            Some(Tok::Use) => {
                self.next();
                let name = self.ident().ok_or(self.err("expected name"))?;
                let mut path = vec![name];
                let mut wild = false;
                while self.peek().as_deref() == Some(&Tok::DoubleColon) {
                    self.next();
                    if let Some(name) = self.ident() {
                        path.push(name);
                    } else {
                        self.expect(Tok::Mul, "'*'")?;
                        wild = true;
                        break;
                    }
                }
                self.expect(Tok::Semicolon, "';'")?;
                let end = path.pop().unwrap();
                let path = RawPath(path, end);

                Ok(Some(PreItem::Use(path, wild)))
            }
            _ => Ok(None),
        };
        if ifdef.resolve(self) {
            i
        } else {
            i?;
            self.item()
        }
    }

    fn stmt(&mut self) -> Result<Option<PreStatement>, Error> {
        self.defines()?;
        let ifdef = self.ifdef()?;
        let i = match self.peek().as_deref() {
            Some(
                Tok::Fn | Tok::Extern | Tok::ExternBlock(_) | Tok::Let | Tok::Enum | Tok::Class,
            ) => Ok(self.item()?.map(PreStatement::Item)),
            Some(Tok::While | Tok::Loop) => {
                let cond = match &*self.next().unwrap() {
                    Tok::While => self.term()?.ok_or(self.err("expected while condition"))?,
                    Tok::Loop => Box::new(Spanned::new(
                        Pre::Lit(Literal::Bool(true), None),
                        self.span(),
                    )),
                    _ => unreachable!(),
                };

                self.expect(Tok::OpenBrace, "'{'")?;
                let mut block = Vec::new();
                loop {
                    if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                        self.next();
                        break;
                    }

                    let stmt = self.stmt()?.ok_or(self.err("expected statement"))?;
                    if matches!(&stmt, PreStatement::Term(x) if x.needs_semicolon()) {
                        self.expect(Tok::Semicolon, "';' after expression statement")?;
                    }
                    block.push(stmt);
                }

                Ok(Some(PreStatement::While(cond, block)))
            }
            Some(Tok::For) => {
                self.next();
                let public = if self.peek().as_deref() == Some(&Tok::Pub) {
                    self.next();
                    true
                } else {
                    false
                };
                let var = self.ident().ok_or(self.err("expected name"))?;

                self.expect(Tok::In, "'in'")?;

                let unroll = if self.peek().as_deref() == Some(&Tok::Unroll) {
                    self.next();
                    true
                } else {
                    false
                };

                let a = self.term()?.ok_or(self.err("expected expression"))?;
                let b = if self.peek().as_deref() == Some(&Tok::DotDot) {
                    self.next();
                    Some(self.term()?.ok_or(self.err("expected expression"))?)
                } else {
                    None
                };

                self.expect(Tok::OpenBrace, "'{'")?;
                let mut block = Vec::new();
                loop {
                    if self.peek().as_deref() == Some(&Tok::CloseBrace) {
                        self.next();
                        break;
                    }

                    let stmt = self.stmt()?.ok_or(self.err("expected statement"))?;
                    if matches!(&stmt, PreStatement::Term(x) if x.needs_semicolon()) {
                        self.expect(Tok::Semicolon, "';' after expression statement")?;
                    }
                    block.push(stmt);
                }

                Ok(Some(PreStatement::For(var, public, unroll, a, b, block)))
            }
            _ => Ok(self.term()?.map(PreStatement::Term)),
        };
        if ifdef.resolve(self) {
            i
        } else {
            i?;
            self.stmt()
        }
    }

    pub fn top_level(&mut self) -> Result<Vec<PreItem>, Error> {
        let mut v = Vec::new();
        while self.peek().is_some() {
            match self.item() {
                Ok(Some(x)) => v.push(x),
                Err(e) => {
                    if let Some(x) = self.next_err.take() {
                        return Err(x);
                    } else {
                        return Err(e);
                    }
                }
                Ok(None) => {
                    if self.peek().is_some() {
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
        }
        match self.next_err.take() {
            Some(x) => Err(x),
            None => Ok(v),
        }
    }
}
