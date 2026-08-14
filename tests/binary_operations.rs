mod utils;

use utils::compile_and_run;

#[test]
fn addition() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(10 + 5)
}
"#
        ),
        "15\n"
    );
}

#[test]
fn subtraction() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(40 - 8)
}
"#
        ),
        "32\n"
    );
}

#[test]
fn multiplication() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(6 * 7)
}
"#
        ),
        "42\n"
    );
}

#[test]
fn signed_division() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(20 / 6)
}
"#
        ),
        "3\n"
    );
}

#[test]
fn signed_modulo() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(20 % 6)
}
"#
        ),
        "2\n"
    );
}

#[test]
fn multiplication_binds_tighter_than_addition() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(1 + 2 * 3 + 5)
}
"#
        ),
        "12\n"
    );
}

#[test]
fn division_and_modulo_bind_tighter_than_subtraction() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(2 + 3 * 4 - 10 / 2)
}
"#
        ),
        "9\n"
    );
}

#[test]
fn subtraction_is_left_associative() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(20 - 5 - 3)
}
"#
        ),
        "12\n"
    );
}

#[test]
fn division_is_left_associative() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(100 / 5 / 2)
}
"#
        ),
        "10\n"
    );
}
