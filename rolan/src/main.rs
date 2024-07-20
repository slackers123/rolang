use roc::bytecode::interpreter::Interpreter;

fn main() {
    let file = std::fs::read_to_string("tests/hello.ro").unwrap();
    let ast = roc::build_file(&file).unwrap();
    // roc::run_ast(ast);

    // roc::bytecode::run_test();

    let bc = roc::bytecode::generator::generate_from_ast(ast);
    let mut int = Interpreter { functions: &bc.1 };
    int.run(bc.0.as_ref().unwrap_or(&"main".to_owned()));
}
