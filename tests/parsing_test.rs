use groenlang::parser::Parser;

#[test]
fn eval_var_decl() {
    let var_decl = String::from("let x=5;");
    Parser::eval(var_decl);
}
