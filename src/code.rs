use crate::lexer::Token;
use crate::object::Value;

#[derive(Debug)]
pub enum BinOpr {
    /* arithmetic operators */
    Add,
    Sub,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    /* bitwise operators */
    Band,
    Bor,
    BXor,
    ShL,
    ShR,
    /* string operator */
    Concat,
    /* comparison operators */
    Eq,
    LT,
    LE,
    NE,
    GT,
    GE,
    /* logical operators */
    And,
    Or,
}

impl BinOpr {
    pub fn try_from(candidate: &Token) -> Option<Self> {
        use BinOpr::*;
        use Token::*;
        return match candidate {
            Char('+') => Some(Add),
            Char('-') => Some(Sub),
            Char('*') => Some(Mul),
            Char('%') => Some(Mod),
            Char('^') => Some(Pow),
            Char('/') => Some(Div),
            Idiv => Some(IDiv),
            Char('&') => Some(Band),
            Char('|') => Some(Bor),
            Char('~') => Some(BXor),
            Shl => Some(ShL),
            Shr => Some(ShR),
            Token::Concat => Some(BinOpr::Concat),
            Ne => Some(NE),
            Token::Eq => Some(BinOpr::Eq),
            Char('<') => Some(LT),
            Le => Some(LE),
            Char('>') => Some(GT),
            Ge => Some(GE),
            Token::And => Some(BinOpr::And),
            Token::Or => Some(BinOpr::Or),
            _ => None,
        };
    }

    pub fn left_priority(&self) -> u8 {
        use BinOpr::*;
        return match &self {
            /* arithmetic operators */
            Add | Sub => 10,
            Mul | Mod => 11,
            Pow => 14,
            Div | IDiv => 11,
            /* bitwise operators */
            Band => 6,
            Bor => 4,
            BXor => 5,
            ShL | ShR => 7,
            /* string operator */
            Concat => 9,
            /* comparison operators */
            Eq | LT | LE | NE | GT | GE => 3,
            /* logical operators */
            And => 2,
            Or => 1,
        };
    }

    pub fn right_priority(&self) -> u8 {
        use BinOpr::*;
        return match &self {
            /* arithmetic operators */
            Add | Sub => 10,
            Mul | Mod => 11,
            Pow => 13,
            Div | IDiv => 11,
            /* bitwise operators */
            Band => 6,
            Bor => 4,
            BXor => 5,
            ShL | ShR => 7,
            /* string operator */
            Concat => 8,
            /* comparison operators */
            Eq | LT | LE | NE | GT | GE => 3,
            /* logical operators */
            And => 2,
            Or => 1,
        };
    }
}

pub enum UnOpr {
    Minus,
    Not,
}

impl UnOpr {
    pub fn try_from(candidate: &Token) -> Option<Self> {
        return match candidate {
            Token::Char('-') => Some(UnOpr::Minus),
            Token::Char('!') => Some(UnOpr::Not),
            _ => None,
        };
    }

    pub fn apply(self, v: &Value) -> Option<Value> {
        use UnOpr::*;
        use Value::*;
        return match self {
            Minus => match v {
                Int(i) => Some(Int(-i)),
                Number(f) => Some(Number(-f)),
                _ => None,
            },
            Not => match v {
                Bool(b) => Some(Bool(!b)),
                _ => None,
            },
        };
    }
}
