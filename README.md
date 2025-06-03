## notun

A toy programming language inspired by Rust

```rust
fn main(a: Int64, b: (Bool, Int64)) -> (Int64, Int64) {
    let a: Int64 = 1 + 2 * 3 + 5
    let b: Int64 = 50 * -99
    let b: Int64 = -a
    let f: Bool = true
    if (f) {
        b = 9
    } else {
        b = 6
    }
    while (f) {
        b = b + 1
        f = false
    }
    return (5, 5)
}
```
