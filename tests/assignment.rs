mod utils;

use utils::compile_and_run;

#[test]
fn let_binding_is_read_back() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 1
    print(a)
}
"#
        ),
        "1\n"
    );
}

#[test]
fn assignment_overwrites_let_binding() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 1
    a = 5
    print(a)
}
"#
        ),
        "5\n"
    );
}
