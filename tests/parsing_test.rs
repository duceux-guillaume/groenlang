use groenlang::error::GResult;
use groenlang::parser::Parser;

#[test]
fn eval_var_decl() -> GResult<()> {
    let var_decl = String::from(
        "
    let a = 0
    let b = -1
    let c = a
    let d = true
    let e = !true
    let f = 5 + 6/2
    let g = (5 + 6)/2
    if false {
        let h = true
    }
    if false {
        let i = false
    } else {
        let i = true
    }
    if false {
        let k = false
    } elseif true {
        let k = true
    }
    if true {
        let l = false
    } elseif false {
        let k = true
    }
    if false {
        let l = false
    } elseif false {
        let k = true
    } else {
        let m = 100
    }
    ",
    );
    return Parser::eval(var_decl);
}

#[test]
fn eval_uop_minus_error() {
    let var_decl = String::from(
        "
    let b = -true
    ",
    );
    let res = Parser::eval(var_decl);
    assert!(!res.is_ok());
}

#[test]
fn eval_unknown_var_error() {
    let var_decl = String::from(
        "
    let b = y
    ",
    );
    let res = Parser::eval(var_decl);
    assert!(!res.is_ok());
}
