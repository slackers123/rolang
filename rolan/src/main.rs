fn main() {
    let file = std::fs::read_to_string("tests/hello.ro").unwrap();
    let ast = roc::build_file(&file).unwrap();
    roc::run_ast(ast);
}
