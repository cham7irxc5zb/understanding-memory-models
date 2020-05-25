use super::{Op, Span, SyntaxError};
use crate::index::*;

use std::str;

define_index_type! {
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct Ident(usize);
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token {
    EOF,
    KwLoc,
    KwProc,
    KwReg,
    KwRun,
    KwIf,
    KwLoop,
    KwElse,
    KwReturn,
    KwBreak,
    KwIgnore,
    KwRead,
    KwWrite,
    KwUpdate,
    KwAcq,
    KwRel,
    KwSc,
    Ident(Ident),
    Int(i64),
    Op(Op),
    Assign,
    From,
    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,
    Comma,
    Semi,
    Error,
}

pub struct Lexer<'a> {
    code: &'a [u8],
    off: usize,
    itab: ISet<Ident, Box<[u8]>>,
    errors: Vec<SyntaxError>,
}

const KEYWORDS: [(Token, &str); 16] = [
    (Token::KwLoc, "loc"),
    (Token::KwProc, "proc"),
    (Token::KwReg, "reg"),
    (Token::KwRun, "run"),
    (Token::KwIf, "if"),
    (Token::KwLoop, "loop"),
    (Token::KwElse, "else"),
    (Token::KwReturn, "return"),
    (Token::KwBreak, "break"),
    (Token::KwIgnore, "ignore"),
    (Token::KwRead, "read"),
    (Token::KwWrite, "write"),
    (Token::KwUpdate, "update"),
    (Token::KwAcq, "acq"),
    (Token::KwRel, "rel"),
    (Token::KwSc, "sc"),
];

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(code: &'a [u8]) -> Self {
        let mut r = Lexer {
            code,
            off: 0,
            itab: ISet::new(),
            errors: Vec::new(),
        };
        for (i, (_, name)) in KEYWORDS.iter().cloned().enumerate() {
            assert_eq!(r.itab.insert(Box::from(name.as_bytes())), Ident(i));
        }
        r
    }

    pub fn ident(&self, i: Ident) -> &str {
        str::from_utf8(self.itab.at(i)).unwrap()
    }

    #[inline]
    fn skip_spaces(&mut self) {
        let n = find_not(self.code, is_space);
        self.off += n;
        self.code = &self.code[n..];
    }

    #[inline]
    fn read_word<C: Fn(u8) -> bool>(&mut self, cond: C) -> &'a [u8] {
        let n = find_not(self.code, cond);
        assert!(n > 0);
        self.off += n;
        let (w, rest) = self.code.split_at(n);
        self.code = rest;
        w
    }

    pub fn errors(&mut self) -> &mut Vec<SyntaxError> {
        &mut self.errors
    }

    pub fn into_errors(self) -> Vec<SyntaxError> {
        self.errors
    }

    fn error(&mut self, off0: usize, msg: &'static str) {
        self.errors.push(SyntaxError {
            span: Span(off0, self.off - 1),
            msg,
        })
    }

    #[inline]
    pub fn next(&mut self) -> (Token, Span) {
        self.skip_spaces();
        let off0 = self.off;
        let c0 = match self.code.first() {
            Some(v) => *v,
            None => return (Token::EOF, Span(off0, off0)),
        };
        let tok = if is_ident(c0) {
            let w = self.read_word(is_ident);
            if is_decimal(w[0]) {
                let w = std::str::from_utf8(w).unwrap();
                match i64::from_str_radix(w, 10) {
                    Ok(i) => Token::Int(i),
                    Err(_) => {
                        self.error(off0, "not a base-10 integer");
                        Token::Error
                    }
                }
            } else {
                match self.itab.entry(w) {
                    ISetEntry::Occupied(e) => {
                        let i = e.index();
                        let ii = i.as_usize();
                        if ii < KEYWORDS.len() {
                            KEYWORDS[ii].0
                        } else {
                            Token::Ident(i)
                        }
                    }
                    ISetEntry::Vacant(e) => Token::Ident(e.insert(Box::from(w)).index()),
                }
            }
        } else if is_op(c0) {
            match self.read_word(is_op) {
                b"+" => Token::Op(Op::Add),
                b"-" => Token::Op(Op::Sub),
                b"*" => Token::Op(Op::Mul),
                b"/" => Token::Op(Op::Div),
                b"%" => Token::Op(Op::Mod),
                b"==" => Token::Op(Op::Eq),
                b"!=" => Token::Op(Op::Ne),
                b"<" => Token::Op(Op::Lt),
                b"<=" => Token::Op(Op::Le),
                b">" => Token::Op(Op::Gt),
                b">=" => Token::Op(Op::Ge),
                b"&&" => Token::Op(Op::LogicalAnd),
                b"||" => Token::Op(Op::LogicalOr),
                b"!" => Token::Op(Op::LogicalNot),
                b"=" => Token::Assign,
                b"<-" => Token::From,
                _ => {
                    self.error(off0, "unknown operator");
                    Token::Error
                }
            }
        } else {
            self.off += 1;
            self.code = &self.code[1..];
            match c0 {
                b'(' => Token::Lparen,
                b')' => Token::Rparen,
                b'[' => Token::Lbracket,
                b']' => Token::Rbracket,
                b'{' => Token::Lbrace,
                b'}' => Token::Rbrace,
                b',' => Token::Comma,
                b';' => Token::Semi,
                _ => {
                    self.error(off0, "unexpected character");
                    Token::Error
                }
            }
        };
        (tok, Span(off0, self.off - 1))
    }
}

#[inline]
fn is_space(v: u8) -> bool {
    v == b'\t' || v == b'\n' || v == b' '
}

#[inline]
fn is_ident(v: u8) -> bool {
    v >= b'0' && v <= b'9' || v >= b'A' && v <= b'Z' || v == b'_' || v >= b'a' && v <= b'z'
}

#[inline]
fn is_decimal(v: u8) -> bool {
    v >= b'0' && v <= b'9'
}

#[inline]
fn is_op(v: u8) -> bool {
    match v {
        b'!' | b'%' | b'&' | b'*' | b'+' | b'-' | b'/' | b'<' | b'=' | b'>' | b'|' => true,
        _ => false,
    }
}

#[inline]
fn find_not(v: &[u8], f: impl Fn(u8) -> bool) -> usize {
    for (i, v) in v.iter().enumerate() {
        if !f(*v) {
            return i;
        }
    }
    v.len()
}

#[test]
fn smoke() {
    let mut l = Lexer::new(&b"word word_2 0 != ! = word 1234 (*"[..]);
    assert_eq!(l.next().0, Token::Ident(Ident(KEYWORDS.len())));
    assert_eq!(l.next().0, Token::Ident(Ident(KEYWORDS.len() + 1)));
    assert_eq!(l.next().0, Token::Int(0));
    assert_eq!(l.next().0, Token::Op(Op::Ne));
    assert_eq!(l.next().0, Token::Op(Op::LogicalNot));
    assert_eq!(l.next().0, Token::Assign);
    assert_eq!(l.next().0, Token::Ident(Ident(KEYWORDS.len())));
    assert_eq!(l.next().0, Token::Int(1234));
    assert_eq!(l.next().0, Token::Lparen);
    assert_eq!(l.next().0, Token::Op(Op::Mul));
    assert_eq!(l.next().0, Token::EOF);
    assert_eq!(l.next().0, Token::EOF);
}

#[test]
fn bad_ident() {
    let mut l = Lexer::new(&b"1a"[..]);
    assert_eq!(l.next(), (Token::Error, Span(0, 1)));
}

#[test]
fn unknown_op() {
    let mut l = Lexer::new(&b"+ ==="[..]);
    assert_eq!(l.next().0, Token::Op(Op::Add));
    assert_eq!(l.next().0, Token::Error);
}
