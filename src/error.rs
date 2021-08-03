pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorType {
    Lexical(),
    Syntactic(),
    Semantic(),
}

#[derive(Debug)]
pub struct Error {
    etype: ErrorType,
    line: usize,
    expected_token: String,
    current_token: String,
}

impl Error {
    pub fn syntactical(l: usize, e: String, c: String) -> Error {
        return Error {
            etype: ErrorType::Syntactic(),
            line: l,
            expected_token: e,
            current_token: c,
        };
    }

    pub fn semantic(l: usize, e: String, c: String) -> Error {
        return Error {
            etype: ErrorType::Semantic(),
            line: l,
            expected_token: e,
            current_token: c,
        };
    }
}
