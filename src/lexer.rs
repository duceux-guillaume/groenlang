use crate::object::Value;
use crate::parser::{Dyndata, FuncState};
use crate::zio::Zio;

use std::rc::Weak;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    TkChar(char),
    /* terminal symbols denoted by reserved words */
    TkAnd,
    TkBreak,
    TkDo,
    TkElse,
    TkElseIf,
    TkEnd,
    TkFalse,
    TkFor,
    TkFunction,
    TkGoto,
    TkIf,
    TkIn,
    TkLocal,
    TkNil,
    TkNot,
    TkOr,
    TkRepeat,
    TkReturn,
    TkThen,
    TkTrue,
    TkUntil,
    TkWhile,
    /* other terminal symbols */
    TkIdiv,
    TkConcat,
    TkDots,
    TkEq,
    TkGe,
    TkLe,
    TkNe,
    TkShl,
    TkShr,
    TkDbcolon,
    TkEos,
    TkFlt(f32),
    TkInt(i32),
    TkName(String),
    TkSring,
}

impl Token {
    fn try_keyword(candidate: &String) -> Option<Self> {
        use Token::*;
        static KEYWORDS: &[(&str, Token)] = &[
            ("and", TkAnd),
            ("break", TkBreak),
            ("do", TkDo),
            ("else", TkElse),
            ("elseif", TkElseIf),
            ("end", TkEnd),
            ("false", TkFalse),
            ("for", TkFor),
            ("function", TkFunction),
            ("goto", TkGoto),
            ("if", TkIf),
            ("in", TkIn),
            ("let", TkLocal),
            ("nil", TkNil),
            ("not", TkNot),
            ("or", TkOr),
            ("repeat", TkRepeat),
            ("return", TkReturn),
            ("then", TkThen),
            ("true", TkTrue),
            ("until", TkUntil),
            ("while", TkWhile),
            ("//", TkIdiv),
            ("..", TkConcat),
            ("...", TkDots),
            ("==", TkEq),
            (">=", TkGe),
            ("<=", TkLe),
            ("~=", TkNe),
            ("<<", TkShl),
            (">>", TkShr),
            ("::", TkDbcolon),
        ];
        for (k, v) in KEYWORDS {
            if *k == candidate {
                return Some(v.clone());
            }
        }
        return None;
    }
}

/* state of the lexer plus state of the parser when shared by all
functions */
pub struct LexState {
    current: char,            /* current character (charint) */
    linenumber: i32,          /* input line counter */
    lastline: i32,            /* line of last token 'consumed' */
    token: Token,             /* current token */
    lookahead: Option<Token>, /* look ahead token */
    pub fs: Weak<FuncState>,  /* current function (parser) */
    //state: Weak<ThreadState>,
    zio: Zio,            /* input stream */
    pub buff: Vec<char>, /* buffer for tokens */
    //h: Table,         /* to avoid collection/reuse strings */
    pub dyd: Dyndata, /* dynamic structures used by the parser */
                      //source: String,   /* current source name */
                      //envn: String      /* environment variable name */
}

impl LexState {
    pub fn new(input: String) -> LexState {
        return LexState {
            current: ' ',
            linenumber: 0,
            lastline: 0,
            token: Token::TkEos,
            lookahead: None,
            fs: Weak::new(),
            //state: Weak::new(),
            zio: Zio::new(input),
            buff: vec![],
            //h: Table::new(),
            dyd: Dyndata::new(),
            //source: String::new(),
        };
    }

    pub fn current(&self) -> Token {
        return self.token.clone();
    }

    pub fn next(&mut self) {
        let old = self.token.clone();
        self.lastline = self.linenumber;
        if let Some(t) = self.lookahead.take() {
            /* discharge it if any */
            /* is there a look-ahead token? */
            self.token = t; /* use this one */
        } else {
            self.token = self.lex() /* read next token */
        }
        println!("lexer:next:{:?}->{:?}", old, self.token);
    }

    fn lex(&mut self) -> Token {
        println!("lexer:lex");
        println!("lexer:current:{}", self.current);
        //luaZ_resetbuffer(ls->buff);
        loop {
            match self.current {
                '\n' | '\r' => self.increment_line_number(),
                ' ' | '\t' => self.next_char(),   /* spaces */
                '-' => return Token::TkChar('-'), //TODO: comment ?
                '[' => return Token::TkChar('['), //TODO: long string ?
                '=' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Token::TkEq;
                    } /* '==' */
                    return Token::TkChar('=');
                }
                '<' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Token::TkLe;
                    } /* '<=' */
                    if matches!(self.current, '<') {
                        return Token::TkShl;
                    } /* '<<' */
                    return Token::TkChar('<');
                }
                '>' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Token::TkGe;
                    } /* '>=' */
                    if matches!(self.current, '>') {
                        return Token::TkShr;
                    } /* '>>' */
                    return Token::TkChar('>');
                }
                '/' => {
                    self.next_char();
                    if matches!(self.current, '/') {
                        return Token::TkIdiv;
                    } /* '//' */
                    return Token::TkChar('/');
                }
                '~' => {
                    //TODO use ! instead ?
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Token::TkNe;
                    } /* '~=' */
                    return Token::TkChar('~');
                }
                ':' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Token::TkDbcolon;
                    } /* '::' */
                    return Token::TkChar(':');
                }
                '\"' | '\'' => {
                    /* short literal strings */
                    //self.read_string()
                    return Token::TkSring;
                }
                '.' => {
                    /* '.', '..', '...', or number */
                    //TODO
                    return Token::TkDots;
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    return self.read_numeral()
                }
                //TODO: EOZ
                _ => {
                    if self.current.is_alphabetic() {
                        /* identifier or reserved word? */
                        loop {
                            self.save_and_next();
                            if !self.current.is_alphanumeric() {
                                break;
                            }
                        }
                        let name = self.collect_buff();
                        let keyword = Token::try_keyword(&name);
                        if let Some(t) = keyword {
                            /* reserved word? */
                            return t.clone();
                        } else {
                            return Token::TkName(name);
                        }
                    }
                    /* single-char tokens ('+', '*', '%', '{', '}', ...) */
                    let t = Token::TkChar(self.current);
                    self.next();
                    return t;
                }
            }
        }
    }

    fn collect_buff(&mut self) -> String {
        let mut s = String::new();
        for c in self.buff.iter() {
            s.push(*c);
        }
        self.buff.clear();
        return s;
    }

    /* NUMBER */
    /*
     ** This function is quite liberal in what it accepts, as 'str2num'
     ** will reject ill-formed numerals. Roughly, it accepts the following
     ** pattern:
     **
     **   %d(%x|%.|([Ee][+-]?))* | 0[Xx](%x|%.|([Pp][+-]?))*
     **
     ** The only tricky part is to accept [+-] only after a valid exponent
     ** mark, to avoid reading '3-4' or '0xe+1' as a single number.
     **
     ** The caller might have already read an initial dot.
     */
    fn read_numeral(&mut self) -> Token {
        println!("lex:read_numeral");
        let first = self.current;
        self.save_and_next();
        let mut expo = "Ee";
        if first == '0' && self.next_is_one_of("xX") {
            /* hexadecimal? */
            expo = "Pp";
        }
        loop {
            if self.next_is_one_of(expo) {
                /* exponent mark? */
                self.next_is_one_of("-+"); /* optional exponent sign */
            } else if self.current.is_ascii_digit() || self.current == '.' {
                /* '%x|%.' */
                self.save_and_next();
            } else {
                break;
            }
        }
        if self.current.is_ascii_alphabetic() {
            /* is numeral touching a letter? */
            self.save_and_next(); /* force an error */ //TODO ? how to handle error ?
        }
        let obj = Value::try_from(&self.buff);
        if obj.is_none() {
            /* format error? */
            self.lexerror();
        }
        if let Some(Value::Int(i)) = obj {
            self.token = Token::TkInt(i);
            return self.token.clone();
        }
        if let Some(Value::Number(n)) = obj {
            self.token = Token::TkFlt(n);
            return self.token.clone();
        }
        self.lexerror();
        return Token::TkInt(0);
    }

    fn next_is_one_of(&mut self, pat: &str) -> bool {
        for c in pat.chars() {
            if self.current == c {
                self.save_and_next();
                return true;
            }
        }
        return false;
    }

    fn save(&mut self, c: char) {
        self.buff.push(c);
    }

    fn next_char(&mut self) {
        let old = self.current;
        self.current = self.zio.next();
        println!("lexer:next_char:{}->{}", old, self.current);
    }

    fn save_and_next(&mut self) {
        self.save(self.current);
        self.next_char();
    }

    /*
     ** increment line number and skips newline sequence (any of
     ** \n, \r, \n\r, or \r\n)
     */
    fn increment_line_number(&mut self) {
        let old = self.current;
        self.next_char(); /* skip '\n' or '\r' */
        if self.current_is_new_line() && self.current != old {
            self.next_char(); /* skip '\n\r' or '\r\n' */
        }
        //TODO?
        //if (++ls->linenumber >= MAX_INT)
        //lexerror(ls, "chunk has too many lines", 0);
    }

    fn current_is_new_line(&self) -> bool {
        matches!(self.current, '\n' | '\r')
    }

    fn lexerror(&self) {
        panic!("byebye error");
    }
}
