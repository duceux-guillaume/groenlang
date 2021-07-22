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
            TkChar('+') => Some(BoAdd),
            TkChar('-') => Some(BoSub),
            TkChar('*') => Some(BoMul),
            TkChar('%') => Some(BoMod),
            TkChar('^') => Some(BoPow),
            TkChar('/') => Some(BoDiv),
            TkIdiv => Some(BoIDiv),
            TkChar('&') => Some(BoBand),
            TkChar('|') => Some(BoBor),
            TkChar('~') => Some(BoBXor),
            TkShl => Some(BoShL),
            TkShr => Some(BoShR),
            TkConcat => Some(BoConcat),
            TkNe => Some(BoNE),
            TkEq => Some(BoEq),
            TkChar('<') => Some(BoLT),
            TkLe => Some(BoLE),
            TkChar('>') => Some(BoGT),
            TkGe => Some(BoGE),
            TkAnd => Some(BoAnd),
            TkOr => Some(BoOr),
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
            TkChar('-') => Some(UoMinus),
            TkChar('~') => Some(UoBNot),
            TkChar('#') => Some(UoLen),
            TkNot => Some(UoNot),
            _ => None,
        };
    }
}
