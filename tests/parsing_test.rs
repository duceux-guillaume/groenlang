use groenlang::parser::Parser;

#[test]
fn eval_var_decl() {
    let var_decl = String::from("let a = 1+3");
    let res = Parser::eval(var_decl);
    println!("{:?}", res);
    assert!(res.is_ok());
}
