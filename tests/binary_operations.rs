mod utils;

use utils::compile_and_run;

#[test]
fn addition() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 10 + 5
}
"#
        ),
        15
    );
}

#[test]
fn subtraction() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 40 - 8
}
"#
        ),
        32
    );
}

#[test]
fn multiplication() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 6 * 7
}
"#
        ),
        42
    );
}

#[test]
fn signed_division() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 20 / 6
}
"#
        ),
        3
    );
}

#[test]
fn signed_modulo() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 20 % 6
}
"#
        ),
        2
    );
}

#[test]
fn multiplication_binds_tighter_than_addition() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 1 + 2 * 3 + 5
}
"#
        ),
        12
    );
}

#[test]
fn division_and_modulo_bind_tighter_than_subtraction() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 2 + 3 * 4 - 10 / 2
}
"#
        ),
        9
    );
}

#[test]
fn subtraction_is_left_associative() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 20 - 5 - 3
}
"#
        ),
        12
    );
}

#[test]
fn division_is_left_associative() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() -> Int64 {
    return 100 / 5 / 2
}
"#
        ),
        10
    );
}
