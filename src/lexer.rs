use crate::error::{Error, GResult};
use crate::zio::Zio;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Char(char),
    /* terminal symbols denoted by reserved words */
    And,
    Break,
    Do,
    Else,
    ElseIf,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Let,
    Nil,
    Or,
    Repeat,
    Return,
    True,
    Until,
    While,
    /* other terminal symbols */
    Idiv,
    Concat,
    Dots,
    Eq,
    Ge,
    Le,
    Ne,
    Shl,
    Shr,
    Dbcolon,
    Eos,
    Flt(String),
    Int(String),
    Name(String),
    Sring,
}

impl Token {
    fn try_keyword(candidate: &String) -> Option<Self> {
        use Token::*;
        static KEYWORDS: &[(&str, Token)] = &[
            ("and", And),
            ("break", Break),
            ("do", Do),
            ("else", Else),
            ("elseif", ElseIf),
            ("false", False),
            ("for", For),
            ("function", Function),
            ("goto", Goto),
            ("if", If),
            ("in", In),
            ("let", Let),
            ("nil", Nil),
            ("or", Or),
            ("repeat", Repeat),
            ("return", Return),
            ("true", True),
            ("until", Until),
            ("while", While),
        ];
        for (k, v) in KEYWORDS {
            if *k == candidate {
                return Some(v.clone());
            }
        }
        return None;
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/* state of the lexer plus state of the parser when shared by all
functions */
pub struct LexState {
    current: char,            /* current character (charint) */
    linenumber: usize,        /* input line counter */
    lastline: usize,          /* line of last token 'consumed' */
    token: Token,             /* current token */
    lookahead: Option<Token>, /* look ahead token */
    //state: Weak<ThreadState>,
    zio: Zio, /* input stream */
    pub buff: Vec<char>, /* buffer for tokens */
              //h: Table,         /* to avoid collection/reuse strings */
              //source: String,   /* current source name */
              //envn: String      /* environment variable name */
}

impl LexState {
    pub fn new(input: String) -> LexState {
        return LexState {
            current: '\n',
            linenumber: 0,
            lastline: 0,
            token: Token::Eos,
            lookahead: None,
            //state: Weak::new(),
            zio: Zio::new(input),
            buff: vec![],
            //h: Table::new(),
            //source: String::new(),
        };
    }

    pub fn linenumber(&self) -> usize {
        return self.linenumber;
    }

    pub fn current(&self) -> Token {
        return self.token.clone();
    }

    pub fn next(&mut self) -> GResult<()> {
        self.lastline = self.linenumber;
        if let Some(t) = self.lookahead.take() {
            /* discharge it if any */
            /* is there a look-ahead token? */
            self.token = t; /* use this one */
        } else {
            self.token = self.lex()?; /* read next token */
        }
        return Ok(());
    }

    fn lex(&mut self) -> GResult<Token> {
        use Token::*;
        //luaZ_resetbuffer(ls->buff);
        loop {
            match self.current {
                '\n' | '\r' => {
                    if !self.increment_line_number() {
                        return Ok(Eos);
                    }
                }
                ' ' | '\t' => {
                    if !self.next_char() {
                        return Ok(Eos);
                    }
                } /* spaces */
                '-' => {
                    if self.next_char() {
                        return Ok(Char('-'));
                    } else {
                        return Err(Error::lexical(
                            self.linenumber,
                            "anything".to_owned(),
                            "eof".to_owned(),
                        ));
                    }
                } //TODO: comment ?
                '[' => return Ok(Char('[')), //TODO: long string ?
                '=' => {
                    if self.next_char() && matches!(self.current, '=') {
                        return Ok(Eq);
                    } /* '==' */
                    return Ok(Char('='));
                }
                '<' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Ok(Le);
                    } /* '<=' */
                    if matches!(self.current, '<') {
                        return Ok(Shl);
                    } /* '<<' */
                    return Ok(Char('<'));
                }
                '>' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Ok(Ge);
                    } /* '>=' */
                    if matches!(self.current, '>') {
                        return Ok(Shr);
                    } /* '>>' */
                    return Ok(Char('>'));
                }
                '/' => {
                    self.next_char();
                    if matches!(self.current, '/') {
                        return Ok(Idiv);
                    } /* '//' */
                    return Ok(Char('/'));
                }
                '!' => {
                    //TODO use ! instead ?
                    if !self.next_char() {
                        return Err(Error::lexical(
                            self.linenumber,
                            "anything".to_owned(),
                            "eof".to_owned(),
                        ));
                    }
                    if self.current == '=' {
                        return Ok(Ne);
                    } /* '!=' */
                    return Ok(Char('!'));
                }
                ':' => {
                    self.next_char();
                    if matches!(self.current, '=') {
                        return Ok(Dbcolon);
                    } /* '::' */
                    return Ok(Char(':'));
                }
                '\"' | '\'' => {
                    /* short literal strings */
                    //self.read_string()
                    return Ok(Sring);
                }
                '.' => {
                    /* '.', '..', '...', or number */
                    //TODO
                    return Ok(Dots);
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    return Ok(self.read_numeral());
                }
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
                            return Ok(t.clone());
                        } else {
                            return Ok(Name(name));
                        }
                    }
                    /* single-char tokens ('+', '*', '%', '{', '}', ...) */
                    let t = Char(self.current);
                    self.next_char();
                    return Ok(t);
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

    fn read_numeral(&mut self) -> Token {
        let mut number_str = String::new();
        while self.current.is_digit(10) {
            number_str.push(self.current);
            if !self.next_char() {
                return Token::Int(number_str.to_owned());
            }
        }
        return Token::Int(number_str.to_owned());
    }

    fn save(&mut self, c: char) {
        self.buff.push(c);
    }

    fn next_char(&mut self) -> bool {
        if let Some(c) = self.zio.next() {
            self.current = c;
            return true;
        }
        self.current = '\n';
        return false;
    }

    fn save_and_next(&mut self) {
        self.save(self.current);
        self.next_char();
    }

    /*
     ** increment line number and skips newline sequence (any of
     ** \n, \r, \n\r, or \r\n)
     */
    fn increment_line_number(&mut self) -> bool {
        let old = self.current;
        if !self.next_char() {
            /* skip '\n' or '\r' */
            return false;
        }
        if self.current_is_new_line() && self.current != old {
            if !self.next_char() {
                /* skip '\n\r' or '\r\n' */
                return false;
            }
        }
        return true;
    }

    fn current_is_new_line(&self) -> bool {
        matches!(self.current, '\n' | '\r')
    }

    pub fn next_if_token(&mut self, c: Token) -> bool {
        if self.token == c {
            self.next();
            return true;
        }
        return false;
    }

    pub fn next_if_char(&mut self, c: char) -> bool {
        return self.next_if_token(Token::Char(c));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_next_char() {
        let mut lexer = LexState::new(String::from("="));
        assert_eq!(lexer.next_char(), true);
        assert_eq!(lexer.next_char(), false);
    }

    #[test]
    fn lexer_increment_line_number() {
        let mut lexer = LexState::new(String::from("="));
        assert_eq!(lexer.increment_line_number(), true);
        assert_eq!(lexer.increment_line_number(), false);
    }

    #[test]
    fn lexer_next_if_token() {
        let mut lexer = LexState::new(String::from("="));
        assert_eq!(lexer.next_if_token(Token::Char('=')), false);
        lexer.next();
        assert_eq!(lexer.next_if_token(Token::Char('=')), true);
    }

    #[test]
    fn lexer_next_if_char() {
        let mut lexer = LexState::new(String::from("="));
        assert_eq!(lexer.next_if_char('='), false);
        lexer.next();
        assert_eq!(lexer.next_if_char('='), true);
    }

    #[test]
    fn lexer_simple_read_numeral() {
        let numeral = String::from("5");
        let mut lexer = LexState::new(numeral.clone());
        lexer.next_char();
        assert_eq!(lexer.read_numeral(), Token::Int(numeral));
    }

    #[test]
    fn lexer_big_int_read_numeral() {
        let numeral = String::from("1023456789");
        let mut lexer = LexState::new(numeral.clone());
        lexer.next_char();
        assert_eq!(lexer.read_numeral(), Token::Int(numeral));
    }

    #[test]
    fn lexer_eos_read_numeral() {
        let mut lexer = LexState::new(String::from("66;"));
        lexer.next_char();
        assert_eq!(lexer.read_numeral(), Token::Int(String::from("66")));
    }
}
