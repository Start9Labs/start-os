fn main() {
    let mut runtime = deno_core::JsRuntime::new(deno_core::RuntimeOptions {
        will_snapshot: true,
        ..Default::default()
    });
    let snapshot = runtime.snapshot();

    let snapshot_slice: &[u8] = &*snapshot;
    println!("Snapshot size: {}", snapshot_slice.len());

    std::fs::write("JS_SNAPSHOT.bin", snapshot_slice).unwrap();
}
