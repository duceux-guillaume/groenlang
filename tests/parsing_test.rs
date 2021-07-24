use groenlang::parser::Parser;

#[test]
fn eval_var_decl() {
    let var_decl = String::from(
        "let x=5;
        let y = 4 + 6
        if x < 0 {
            x = 0
        }",
    );
    let res = Parser::eval(var_decl);
    println!("{:?}", res);
    assert!(res.is_ok());
}
