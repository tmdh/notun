mod utils;

use utils::compile_and_run;

#[test]
fn a_tuple_field_can_be_read_by_index() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let point: (Int64, Int64) = (3, 4)
    print(point.0)
}
"#
        ),
        "3\n"
    );
}

#[test]
fn the_last_field_of_a_tuple_can_be_read() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let point: (Int64, Int64) = (3, 4)
    print(point.1)
}
"#
        ),
        "4\n"
    );
}

#[test]
fn every_field_of_a_three_element_tuple_is_readable() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let triple: (Int64, Int64, Int64) = (1, 20, 300)
    print(triple.0 + triple.1 + triple.2)
}
"#
        ),
        "321\n"
    );
}

#[test]
fn tuple_fields_can_be_arbitrary_expressions() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let x: Int64 = 5
    let pair: (Int64, Int64) = (x + 1, x * 2)
    print(pair.1)
}
"#
        ),
        "10\n"
    );
}

#[test]
fn a_field_read_is_usable_inside_a_larger_expression() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let pair: (Int64, Int64) = (4, 3)
    print(pair.0 * pair.0 + 2 * pair.1)
}
"#
        ),
        "22\n"
    );
}

#[test]
fn a_tuple_can_mix_an_int64_and_a_float64() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let mixed: (Int64, Float64) = (7, 9.5)
    print(mixed.0)
    if mixed.1 > 4.0 {
        print(1)
    } else {
        print(2)
    }
}
"#
        ),
        "7\n1\n"
    );
}

#[test]
fn a_bool_field_can_drive_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let flagged: (Bool, Int64) = (false, 42)
    if flagged.0 {
        print(flagged.1)
    } else {
        print(0)
    }
}
"#
        ),
        "0\n"
    );
}

#[test]
fn each_field_of_a_mixed_tuple_lands_in_its_own_slot() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let mixed: (Int64, Float64, Bool) = (11, 2.5, true)
    print(mixed.0)
    if mixed.1 < 3.0 {
        print(1)
    } else {
        print(2)
    }
    if mixed.2 {
        print(3)
    } else {
        print(4)
    }
}
"#
        ),
        "11\n1\n3\n"
    );
}

#[test]
fn a_function_can_return_a_tuple() {
    assert_eq!(
        compile_and_run(
            r#"
fn origin() -> (Int64, Float64) {
    return (5, 6.6)
}

fn main() {
    let point: (Int64, Float64) = origin()
    print(point.0)
}
"#
        ),
        "5\n"
    );
}

#[test]
fn a_field_can_be_read_directly_off_a_call_result() {
    assert_eq!(
        compile_and_run(
            r#"
fn make() -> (Int64, Int64) {
    return (8, 9)
}

fn main() {
    print(make().1)
}
"#
        ),
        "9\n"
    );
}

#[test]
fn a_function_can_take_a_tuple_parameter() {
    assert_eq!(
        compile_and_run(
            r#"
fn sum(pair: (Int64, Int64)) -> Int64 {
    return pair.0 + pair.1
}

fn main() {
    let pair: (Int64, Int64) = (13, 29)
    print(sum(pair))
}
"#
        ),
        "42\n"
    );
}

#[test]
fn a_tuple_can_be_passed_in_and_returned_again() {
    assert_eq!(
        compile_and_run(
            r#"
fn swap(pair: (Int64, Int64)) -> (Int64, Int64) {
    return (pair.1, pair.0)
}

fn main() {
    let pair: (Int64, Int64) = (1, 2)
    let swapped: (Int64, Int64) = swap(pair)
    print(swapped.0)
}
"#
        ),
        "2\n"
    );
}

#[test]
fn a_tuple_literal_can_be_written_inline_as_an_argument() {
    assert_eq!(
        compile_and_run(
            r#"
fn first(pair: (Int64, Int64)) -> Int64 {
    return pair.0
}

fn main() {
    print(first((17, 4)))
}
"#
        ),
        "17\n"
    );
}

#[test]
fn binding_a_tuple_to_another_variable_copies_it() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: (Int64, Int64) = (1, 2)
    let b: (Int64, Int64) = a
    a = (9, 9)
    print(b.0)
}
"#
        ),
        "1\n"
    );
}

#[test]
fn a_tuple_variable_can_be_reassigned_as_a_whole() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: (Int64, Int64) = (1, 2)
    a = (7, 8)
    print(a.0 + a.1)
}
"#
        ),
        "15\n"
    );
}

#[test]
fn reassigning_a_tuple_parameter_does_not_affect_the_caller() {
    assert_eq!(
        compile_and_run(
            r#"
fn consume(pair: (Int64, Int64)) -> Int64 {
    pair = (0, 0)
    return pair.0
}

fn main() {
    let pair: (Int64, Int64) = (6, 7)
    let ignored: Int64 = consume(pair)
    print(pair.0)
}
"#
        ),
        "6\n"
    );
}

#[test]
fn a_tuple_can_hold_another_tuple() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let nested: ((Int64, Int64), Int64) = ((10, 20), 30)
    let inner: (Int64, Int64) = nested.0
    print(inner.1 + nested.1)
}
"#
        ),
        "50\n"
    );
}

#[test]
fn a_field_read_can_appear_in_a_while_condition() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let limits: (Int64, Int64) = (0, 20)
    let i: Int64 = 0
    while i < limits.1 {
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
fn a_tuple_can_be_built_fresh_on_every_loop_iteration() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let i: Int64 = 0
    let total: Int64 = 0
    while i < 1000 {
        let step: (Int64, Int64) = (i, i * 2)
        total = total + step.1
        i = i + 1
    }
    print(total)
}
"#
        ),
        "999000\n"
    );
}

#[test]
fn tuples_can_be_built_and_read_inside_both_branches_of_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let x: Int64 = 3
    if x > 1 {
        let taken: (Int64, Int64) = (x, x + 10)
        print(taken.1)
    } else {
        let skipped: (Int64, Int64) = (x, x - 10)
        print(skipped.1)
    }
}
"#
        ),
        "13\n"
    );
}
