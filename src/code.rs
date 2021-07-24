use crate::lexer::Token;

pub enum BinOpr {
    /* arithmetic operators */
    BoAdd,
    BoSub,
    BoMul,
    BoMod,
    BoPow,
    BoDiv,
    BoIDiv,
    /* bitwise operators */
    BoBand,
    BoBor,
    BoBXor,
    BoShL,
    BoShR,
    /* string operator */
    BoConcat,
    /* comparison operators */
    BoEq,
    BoLT,
    BoLE,
    BoNE,
    BoGT,
    BoGE,
    /* logical operators */
    BoAnd,
    BoOr,
}

impl BinOpr {
    pub fn try_from(candidate: &Token) -> Option<Self> {
        use BinOpr::*;
        use Token::*;
        return match candidate {
            Char('+') => Some(BoAdd),
            Char('-') => Some(BoSub),
            Char('*') => Some(BoMul),
            Char('%') => Some(BoMod),
            Char('^') => Some(BoPow),
            Char('/') => Some(BoDiv),
            Idiv => Some(BoIDiv),
            Char('&') => Some(BoBand),
            Char('|') => Some(BoBor),
            Char('~') => Some(BoBXor),
            Shl => Some(BoShL),
            Shr => Some(BoShR),
            Concat => Some(BoConcat),
            Ne => Some(BoNE),
            Eq => Some(BoEq),
            Char('<') => Some(BoLT),
            Le => Some(BoLE),
            Char('>') => Some(BoGT),
            Ge => Some(BoGE),
            And => Some(BoAnd),
            Or => Some(BoOr),
            _ => None,
        };
    }

    pub fn left_priority(&self) -> u8 {
        use BinOpr::*;
        return match &self {
            /* arithmetic operators */
            BoAdd | BoSub => 10,
            BoMul | BoMod => 11,
            BoPow => 14,
            BoDiv | BoIDiv => 11,
            /* bitwise operators */
            BoBand => 6,
            BoBor => 4,
            BoBXor => 5,
            BoShL | BoShR => 7,
            /* string operator */
            BoConcat => 9,
            /* comparison operators */
            BoEq | BoLT | BoLE | BoNE | BoGT | BoGE => 3,
            /* logical operators */
            BoAnd => 2,
            BoOr => 1,
        };
    }
}

pub enum UnOpr {
    UoMinus,
    UoBNot,
    UoNot,
    UoLen,
}

impl UnOpr {
    pub fn try_from(candidate: &Token) -> Option<Self> {
        use Token::*;
        use UnOpr::*;
        return match candidate {
            Char('-') => Some(UoMinus),
            Char('~') => Some(UoBNot),
            Char('#') => Some(UoLen),
            Not => Some(UoNot),
            _ => None,
        };
    }
}
