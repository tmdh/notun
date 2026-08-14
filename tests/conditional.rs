mod utils;

use utils::compile_and_run;

#[test]
fn if_takes_then_branch_when_condition_is_true() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 1
    if a < 5 {
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
fn if_takes_else_branch_when_condition_is_false() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 10
    if a < 5 {
        print(1)
    } else {
        print(2)
    }
}
"#
        ),
        "2\n"
    );
}

#[test]
fn if_without_else_runs_then_branch() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 1
    if a == 1 {
        a = 7
    }
    print(a)
}
"#
        ),
        "7\n"
    );
}

#[test]
fn if_without_else_falls_through_when_condition_is_false() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 1
    if a != 1 {
        a = 7
    }
    print(a)
}
"#
        ),
        "1\n"
    );
}

#[test]
fn execution_continues_after_a_branch_that_does_not_return() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 3
    if a >= 3 {
        a = a * 2
    } else {
        a = 0
    }
    print(a + 1)
}
"#
        ),
        "7\n"
    );
}

#[test]
fn if_condition_reads_a_bool_variable() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let flag: Bool = false
    if flag {
        print(1)
    } else {
        print(2)
    }
}
"#
        ),
        "2\n"
    );
}

#[test]
fn if_condition_combines_comparisons_with_logical_operators() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 4
    if a > 0 && a <= 4 {
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
fn nested_if_inside_then_branch() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 8
    if a > 5 {
        if a % 2 == 0 {
            print(10)
        } else {
            print(20)
        }
    } else {
        print(30)
    }
}
"#
        ),
        "10\n"
    );
}

#[test]
fn nested_if_inside_else_branch_acts_as_else_if() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 5
    if a < 5 {
        print(1)
    } else {
        if a == 5 {
            print(2)
        } else {
            print(3)
        }
    }
}
"#
        ),
        "2\n"
    );
}

#[test]
fn while_loop_accumulates_a_sum() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let i: Int64 = 1
    let total: Int64 = 0
    while i <= 5 {
        total = total + i
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
fn while_body_is_skipped_when_condition_is_false_initially() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 100
    while a < 10 {
        a = a + 1
    }
    print(a)
}
"#
        ),
        "100\n"
    );
}

#[test]
fn while_condition_reads_a_bool_variable() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let running: Bool = true
    let count: Int64 = 0
    while running {
        count = count + 1
        running = false
    }
    print(count)
}
"#
        ),
        "1\n"
    );
}

#[test]
fn return_inside_while_body_exits_immediately() {
    assert_eq!(
        compile_and_run(
            r#"
fn program() -> Int64 {
    let a: Int64 = 99
    while a < 150 {
        return a
    }
    return 0
}

fn main() {
    print(program())
}
"#
        ),
        "99\n"
    );
}

#[test]
fn nested_while_loops_multiply_iteration_counts() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let i: Int64 = 0
    let count: Int64 = 0
    while i < 3 {
        let j: Int64 = 0
        while j < 4 {
            count = count + 1
            j = j + 1
        }
        i = i + 1
    }
    print(count)
}
"#
        ),
        "12\n"
    );
}

#[test]
fn if_inside_while_counts_matching_values() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let i: Int64 = 1
    let evens: Int64 = 0
    while i <= 10 {
        if i % 2 == 0 {
            evens = evens + 1
        }
        i = i + 1
    }
    print(evens)
}
"#
        ),
        "5\n"
    );
}

#[test]
fn if_inside_while_returns_out_of_the_loop() {
    assert_eq!(
        compile_and_run(
            r#"
fn program() -> Int64 {
    let i: Int64 = 0
    while i < 100 {
        if i * i > 50 {
            return i
        }
        i = i + 1
    }
    return 0
}

fn main() {
    print(program())
}
"#
        ),
        "8\n"
    );
}

#[test]
fn while_inside_if_runs_only_on_the_taken_branch() {
    assert_eq!(
        compile_and_run(
            r#"
fn main() {
    let a: Int64 = 2
    let result: Int64 = 1
    if a > 1 {
        let i: Int64 = 0
        while i < 5 {
            result = result * 2
            i = i + 1
        }
    } else {
        result = 0
    }
    print(result)
}
"#
        ),
        "32\n"
    );
}
