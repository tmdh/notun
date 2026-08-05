use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn compile_and_run(source: &str) -> i32 {
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir().join(format!("notun-test-{}-{}", std::process::id(), id));
    std::fs::create_dir_all(&dir).unwrap();
    let src_path = dir.join("prog.nt");
    std::fs::write(&src_path, source).unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_notun"))
        .current_dir(&dir)
        .arg("build")
        .arg(&src_path)
        .output()
        .expect("failed to invoke the notun compiler");
    assert!(
        output.status.success(),
        "compiler process failed for source:\n{source}\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    let exe = dir.join("notun-cache").join("program");
    assert!(
        exe.exists(),
        "compiler did not produce an executable for source:\n{source}\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout),
    );

    let status = Command::new(&exe)
        .status()
        .expect("failed to run the compiled program");
    let code = status.code().expect("compiled program terminated by a signal");

    let _ = std::fs::remove_dir_all(&dir);
    code
}
