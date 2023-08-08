use deno_core::JsRuntimeForSnapshot;

fn main() {
    let runtime = JsRuntimeForSnapshot::new(Default::default(), Default::default());
    let snapshot = runtime.snapshot();

    let snapshot_slice: &[u8] = &*snapshot;
    println!("Snapshot size: {}", snapshot_slice.len());

    std::fs::write("JS_SNAPSHOT.bin", snapshot_slice).unwrap();
}
