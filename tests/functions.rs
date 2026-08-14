mod utils;

use utils::compile_and_run;

#[test]
fn call_a_function_that_takes_no_parameters() {
    assert_eq!(
        compile_and_run(
            r#"
fn answer() -> Int64 {
    return 42
}

fn main() {
    print(answer())
}
"#
        ),
        "42\n"
    );
}

#[test]
fn call_a_function_with_a_single_parameter() {
    assert_eq!(
        compile_and_run(
            r#"
fn double(a: Int64) -> Int64 {
    return a * 2
}

fn main() {
    print(double(21))
}
"#
        ),
        "42\n"
    );
}

#[test]
fn call_a_function_with_several_parameters() {
    assert_eq!(
        compile_and_run(
            r#"
fn sum3(a: Int64, b: Int64, c: Int64) -> Int64 {
    return a + b + c
}

fn main() {
    print(sum3(1, 2, 3))
}
"#
        ),
        "6\n"
    );
}

#[test]
fn arguments_are_bound_to_parameters_in_order() {
    assert_eq!(
        compile_and_run(
            r#"
fn subtract(a: Int64, b: Int64) -> Int64 {
    return a - b
}

fn main() {
    print(subtract(30, 8))
}
"#
        ),
        "22\n"
    );
}

#[test]
fn call_result_is_usable_inside_a_larger_expression() {
    assert_eq!(
        compile_and_run(
            r#"
fn square(a: Int64) -> Int64 {
    return a * a
}

fn main() {
    print(square(4) + 2 * square(3))
}
"#
        ),
        "34\n"
    );
}

#[test]
fn call_result_is_usable_as_an_argument_to_another_call() {
    assert_eq!(
        compile_and_run(
            r#"
fn double(a: Int64) -> Int64 {
    return a * 2
}

fn increment(a: Int64) -> Int64 {
    return a + 1
}

fn main() {
    print(double(increment(double(5))))
}
"#
        ),
        "22\n"
    );
}

#[test]
fn call_result_is_bound_by_a_let_statement() {
    assert_eq!(
        compile_and_run(
            r#"
fn triple(a: Int64) -> Int64 {
    return a * 3
}

fn main() {
    let value: Int64 = triple(7)
    print(value + 1)
}
"#
        ),
        "22\n"
    );
}

#[test]
fn call_result_is_usable_on_the_right_hand_side_of_an_assignment() {
    assert_eq!(
        compile_and_run(
            r#"
fn negate_offset(a: Int64) -> Int64 {
    return 100 - a
}

fn main() {
    let value: Int64 = 1
    value = negate_offset(90)
    print(value)
}
"#
        ),
        "10\n"
    );
}

#[test]
fn arguments_can_be_arbitrary_expressions() {
    assert_eq!(
        compile_and_run(
            r#"
fn add(a: Int64, b: Int64) -> Int64 {
    return a + b
}

fn main() {
    let x: Int64 = 4
    print(add(x * 2, x + 3))
}
"#
        ),
        "15\n"
    );
}

#[test]
fn a_function_can_be_called_more_than_once() {
    assert_eq!(
        compile_and_run(
            r#"
fn double(a: Int64) -> Int64 {
    return a * 2
}

fn main() {
    print(double(1) + double(2) + double(3))
}
"#
        ),
        "12\n"
    );
}

#[test]
fn a_function_returning_bool_can_drive_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn is_even(a: Int64) -> Bool {
    return a % 2 == 0
}

fn main() {
    if is_even(10) {
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
fn a_function_can_take_a_bool_parameter() {
    assert_eq!(
        compile_and_run(
            r#"
fn pick(flag: Bool) -> Int64 {
    if flag {
        return 10
    } else {
        return 20
    }
}

fn main() {
    print(pick(false))
}
"#
        ),
        "20\n"
    );
}

#[test]
fn a_function_can_take_and_return_float64() {
    assert_eq!(
        compile_and_run(
            r#"
fn half(a: Float64) -> Float64 {
    return a / 2.0
}

fn main() {
    if half(9.0) > 4.0 {
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
fn a_function_body_can_use_control_flow() {
    assert_eq!(
        compile_and_run(
            r#"
fn sum_to(n: Int64) -> Int64 {
    let i: Int64 = 1
    let total: Int64 = 0
    while i <= n {
        total = total + i
        i = i + 1
    }
    return total
}

fn main() {
    print(sum_to(10))
}
"#
        ),
        "55\n"
    );
}

#[test]
fn a_parameter_can_be_reassigned_inside_the_callee() {
    assert_eq!(
        compile_and_run(
            r#"
fn clamp_low(a: Int64) -> Int64 {
    if a < 0 {
        a = 0
    }
    return a
}

fn main() {
    print(clamp_low(0 - 5) + clamp_low(7))
}
"#
        ),
        "7\n"
    );
}

#[test]
fn reassigning_a_parameter_does_not_affect_the_caller() {
    assert_eq!(
        compile_and_run(
            r#"
fn consume(a: Int64) -> Int64 {
    a = 0
    return a
}

fn main() {
    let x: Int64 = 9
    let ignored: Int64 = consume(x)
    print(x)
}
"#
        ),
        "9\n"
    );
}

#[test]
fn callee_locals_do_not_clobber_caller_locals_of_the_same_name() {
    assert_eq!(
        compile_and_run(
            r#"
fn helper(a: Int64) -> Int64 {
    let value: Int64 = 1000
    return a + value
}

fn main() {
    let value: Int64 = 5
    let ignored: Int64 = helper(2)
    print(value)
}
"#
        ),
        "5\n"
    );
}

#[test]
fn a_call_can_appear_inside_a_while_body() {
    assert_eq!(
        compile_and_run(
            r#"
fn square(a: Int64) -> Int64 {
    return a * a
}

fn main() {
    let i: Int64 = 1
    let total: Int64 = 0
    while i <= 4 {
        total = total + square(i)
        i = i + 1
    }
    print(total)
}
"#
        ),
        "30\n"
    );
}

#[test]
fn a_call_can_appear_inside_a_while_condition() {
    assert_eq!(
        compile_and_run(
            r#"
fn below_limit(a: Int64) -> Bool {
    return a < 20
}

fn main() {
    let i: Int64 = 0
    while below_limit(i) {
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
fn a_recursive_function_computes_a_factorial() {
    assert_eq!(
        compile_and_run(
            r#"
fn factorial(n: Int64) -> Int64 {
    if n <= 1 {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

fn main() {
    print(factorial(5))
}
"#
        ),
        "120\n"
    );
}

#[test]
fn a_recursive_function_computes_a_fibonacci_number() {
    assert_eq!(
        compile_and_run(
            r#"
fn fib(n: Int64) -> Int64 {
    if n < 2 {
        return n
    } else {
        return fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    print(fib(12))
}
"#
        ),
        "144\n"
    );
}

#[test]
fn a_recursive_function_recurses_over_two_parameters() {
    assert_eq!(
        compile_and_run(
            r#"
fn gcd(a: Int64, b: Int64) -> Int64 {
    if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}

fn main() {
    print(gcd(48, 18))
}
"#
        ),
        "6\n"
    );
}

#[test]
fn a_recursive_function_can_bottom_out_immediately() {
    assert_eq!(
        compile_and_run(
            r#"
fn countdown(n: Int64) -> Int64 {
    if n <= 0 {
        return 0
    } else {
        return 1 + countdown(n - 1)
    }
}

fn main() {
    print(countdown(0) + countdown(6))
}
"#
        ),
        "6\n"
    );
}

#[test]
fn a_call_chain_threads_through_several_helpers() {
    assert_eq!(
        compile_and_run(
            r#"
fn add(a: Int64, b: Int64) -> Int64 {
    return a + b
}

fn scale(a: Int64) -> Int64 {
    return add(a, a) + add(a, 0)
}

fn main() {
    print(scale(9))
}
"#
        ),
        "27\n"
    );
}

#[test]
fn a_function_can_call_one_declared_later_in_the_file() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(later(3))
}

fn later(a: Int64) -> Int64 {
    return a * 4
}
"#
        ),
        "12\n"
    );
}

#[test]
fn forward_references_work_through_a_chain_of_declarations() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    print(first(2))
}

fn first(a: Int64) -> Int64 {
    return second(a) + 1
}

fn second(a: Int64) -> Int64 {
    return third(a) * 10
}

fn third(a: Int64) -> Int64 {
    return a + 3
}
"#
        ),
        "51\n"
    );
}

#[test]
fn two_functions_can_call_each_other_in_both_directions() {
    assert_eq!(
        compile_and_run(
            r#"
fn ping(a: Int64) -> Int64 {
    return pong(a) + 1
}

fn pong(a: Int64) -> Int64 {
    return a * 2
}

fn main() {
    print(ping(5) + pong(ping(1)))
}
"#
        ),
        "17\n"
    );
}

#[test]
fn mutually_recursive_functions_decide_parity() {
    assert_eq!(
        compile_and_run(
            r#"
fn is_even(n: Int64) -> Bool {
    if n == 0 {
        return true
    } else {
        return is_odd(n - 1)
    }
}

fn is_odd(n: Int64) -> Bool {
    if n == 0 {
        return false
    } else {
        return is_even(n - 1)
    }
}

fn main() {
    if is_even(10) && is_odd(7) {
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
fn mutual_recursion_alternates_between_two_accumulators() {
    assert_eq!(
        compile_and_run(
            r#"
fn take(n: Int64) -> Int64 {
    if n <= 0 {
        return 0
    } else {
        return n + skip(n - 1)
    }
}

fn skip(n: Int64) -> Int64 {
    if n <= 0 {
        return 0
    } else {
        return take(n - 1)
    }
}

fn main() {
    print(take(9))
}
"#
        ),
        "25\n"
    );
}

#[test]
fn a_call_can_stand_alone_as_a_statement() {
    assert_eq!(
        compile_and_run(
            r#"
fn side(a: Int64) -> Int64 {
    return a * 2
}

fn main() {
    side(4)
    print(7)
}
"#
        ),
        "7\n"
    );
}

#[test]
fn a_call_statement_takes_no_parameters() {
    assert_eq!(
        compile_and_run(
            r#"
fn nothing() -> Int64 {
    return 1
}

fn main() {
    nothing()
    print(3)
}
"#
        ),
        "3\n"
    );
}

#[test]
fn a_call_statement_evaluates_its_argument_expressions() {
    assert_eq!(
        compile_and_run(
            r#"
fn identity(a: Int64) -> Int64 {
    return a
}

fn main() {
    let x: Int64 = 5
    identity(x * 2 + 1)
    print(x)
}
"#
        ),
        "5\n"
    );
}

#[test]
fn a_discarded_call_statement_does_not_disturb_the_surrounding_locals() {
    assert_eq!(
        compile_and_run(
            r#"
fn helper(a: Int64) -> Int64 {
    let scratch: Int64 = 999
    a = a + scratch
    return a
}

fn main() {
    let scratch: Int64 = 6
    let total: Int64 = 1
    helper(scratch)
    total = total + scratch
    print(total)
}
"#
        ),
        "7\n"
    );
}

#[test]
fn a_call_statement_can_discard_a_bool_result() {
    assert_eq!(
        compile_and_run(
            r#"
fn is_positive(a: Int64) -> Bool {
    return a > 0
}

fn main() {
    is_positive(3)
    print(8)
}
"#
        ),
        "8\n"
    );
}

#[test]
fn a_call_statement_can_discard_a_float64_result() {
    assert_eq!(
        compile_and_run(
            r#"
fn half(a: Float64) -> Float64 {
    return a / 2.0
}

fn main() {
    half(5.0)
    print(4)
}
"#
        ),
        "4\n"
    );
}

#[test]
fn call_statements_can_appear_inside_both_branches_of_an_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn note(a: Int64) -> Int64 {
    return a
}

fn main() {
    let a: Int64 = 3
    if a > 1 {
        note(a)
        a = a + 10
    } else {
        note(0)
        a = 0
    }
    print(a)
}
"#
        ),
        "13\n"
    );
}

#[test]
fn a_call_statement_inside_a_while_body_leaves_the_loop_intact() {
    assert_eq!(
        compile_and_run(
            r#"
fn note(a: Int64) -> Int64 {
    return a * 100
}

fn main() {
    let i: Int64 = 0
    while i < 5 {
        note(i)
        i = i + 1
    }
    print(i)
}
"#
        ),
        "5\n"
    );
}

#[test]
fn a_recursive_call_statement_still_terminates() {
    assert_eq!(
        compile_and_run(
            r#"
fn countdown(n: Int64) -> Int64 {
    if n > 0 {
        countdown(n - 1)
    }
    return n
}

fn main() {
    print(countdown(4))
}
"#
        ),
        "4\n"
    );
}

#[test]
fn a_call_statement_can_target_a_function_declared_later() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    later(2)
    print(9)
}

fn later(a: Int64) -> Int64 {
    return a + 1
}
"#
        ),
        "9\n"
    );
}
