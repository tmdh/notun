mod utils;

use utils::compile_and_run;

#[test]
fn let_binding_is_read_back() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    let a: Int64 = 1
    return a
}
"#
        ),
        1
    );
}

#[test]
fn assignment_overwrites_let_binding() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    let a: Int64 = 1
    a = 5
    return a
}
"#
        ),
        5
    );
}
