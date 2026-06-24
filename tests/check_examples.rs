use homer::build::{self, Compiler};
use std::sync::Arc;

#[test]
fn examples_pass_checker() -> anyhow::Result<()> {
    let examples_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples");
    let mut paths = Vec::new();
    for entry in std::fs::read_dir(&examples_dir)? {
        let path = entry?.path();
        if path.extension().is_some_and(|ext| ext == "doh") {
            paths.push(path);
        }
    }
    paths.sort();

    assert!(!paths.is_empty(), "no .doh files found in examples/");

    for path in paths {
        let input = std::fs::read_to_string(&path)?;
        let db = &mut build::CompilerDB::new();
        let uri = build::Uri::new(path.to_str().unwrap());
        db.set_input(uri, Arc::new(input.clone()));

        let mut diagnostics = Vec::new();
        db.with_diagnostics(uri, |diags| {
            diagnostics.extend(diags.map(|d| d.layout(&input)));
        });

        assert!(
            diagnostics.is_empty(),
            "{} failed with diagnostics:\n{}",
            path.display(),
            diagnostics.join("\n"),
        );
    }
    Ok(())
}
