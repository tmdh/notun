mod utils;

use utils::compile_and_run;

#[test]
fn static_array_sum_matches_test7_reference() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[3] = [1, 5, 9]
    print(a[0] + a[1] + a[2])
}
"#
        ),
        "15\n"
    );
}

#[test]
fn each_index_of_a_three_element_array_is_readable() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[3] = [1, 5, 9]
    print(a[0])
    print(a[1])
    print(a[2])
}
"#
        ),
        "1\n5\n9\n"
    );
}

#[test]
fn single_element_array_can_be_read() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[1] = [42]
    print(a[0])
}
"#
        ),
        "42\n"
    );
}

#[test]
fn five_element_array_sum() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[5] = [1, 2, 3, 4, 5]
    print(a[0] + a[1] + a[2] + a[3] + a[4])
}
"#
        ),
        "15\n"
    );
}

#[test]
fn array_elements_can_be_arbitrary_expressions() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let x: Int64 = 5
    let a: Int64[3] = [x + 1, x * 2, x - 1]
    print(a[1])
}
"#
        ),
        "10\n"
    );
}

#[test]
fn subscript_result_is_usable_inside_larger_expression() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let pair: Int64[2] = [4, 3]
    print(pair[0] * pair[0] + 2 * pair[1])
}
"#
        ),
        "22\n"
    );
}

#[test]
fn index_can_be_a_variable_and_computed_expression() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[3] = [10, 20, 30]
    let i: Int64 = 1
    print(a[i] + a[i + 1])
}
"#
        ),
        "50\n"
    );
}

#[test]
fn while_loop_can_accumulate_over_an_array() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[3] = [1, 5, 9]
    let i: Int64 = 0
    let total: Int64 = 0
    while i < 3 {
        total = total + a[i]
        i = i + 1
    }
    print(total)
}
"#
        ),
        "15\n"
    );
}

#[test]
fn array_element_can_drive_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64[2] = [10, 3]
    if a[0] > a[1] {
        print(1)
    } else {
        print(2)
    }
}
"#
        ),
        "1\n"
    );
}

#[test]
fn array_read_in_while_condition_or_body() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let limits: Int64[2] = [0, 20]
    let i: Int64 = 0
    while i < limits[1] {
        i = i + 3
    }
    print(i)
}
"#
        ),
        "21\n"
    );
}

#[test]
fn a_function_can_take_an_array_parameter() {
    assert_eq!(
        compile_and_run(
            r#"
fn sum(a: Int64[3]) -> Int64 {
    return a[0] + a[1] + a[2]
}

fn main() {
    let a: Int64[3] = [1, 5, 9]
    print(sum(a))
}
"#
        ),
        "15\n"
    );
}

#[test]
fn a_function_can_return_an_array() {
    assert_eq!(
        compile_and_run(
            r#"
fn make() -> Int64[2] {
    return [8, 9]
}

fn main() {
    let a: Int64[2] = make()
    print(a[0] + a[1])
}
"#
        ),
        "17\n"
    );
}

#[test]
fn call_result_is_usable_as_subscript_base() {
    assert_eq!(
        compile_and_run(
            r#"
fn make() -> Int64[2] {
    return [8, 9]
}

fn main() {
    print(make()[1])
}
"#
        ),
        "9\n"
    );
}

#[test]
fn arguments_can_be_arbitrary_index_expressions() {
    assert_eq!(
        compile_and_run(
            r#"
fn add(a: Int64, b: Int64) -> Int64 {
    return a + b
}

fn main() {
    let a: Int64[4] = [3, 7, 11, 19]
    let i: Int64 = 1
    print(add(a[i * 2], a[i + 2]))
}
"#
        ),
        "30\n"
    );
}

#[test]
fn float64_array_elements_are_readable_via_comparison() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let f: Float64[2] = [7.5, 2.5]
    if f[0] > 4.0 {
        print(1)
    } else {
        print(2)
    }
    if f[1] < 3.0 {
        print(3)
    } else {
        print(4)
    }
}
"#
        ),
        "1\n3\n"
    );
}

#[test]
fn bool_array_element_can_drive_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let flags: Bool[2] = [true, false]
    if flags[0] {
        print(1)
    } else {
        print(2)
    }
    if flags[1] {
        print(3)
    } else {
        print(4)
    }
}
"#
        ),
        "1\n4\n"
    );
}

#[test]
fn nested_array_read() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let m: Int64[2][2] = [[1, 2], [3, 4]]
    print(m[0][1] + m[1][0])
}
"#
        ),
        "5\n"
    );
}
