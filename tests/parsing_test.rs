use groenlang::parser::Parser;

#[test]
fn eval_var_decl() {
    let var_decl = String::from(
        "
    let a = 0
    let b = -1
    let c = a
    ",
    );
    let res = Parser::eval(var_decl);
    assert!(res.is_ok());
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
