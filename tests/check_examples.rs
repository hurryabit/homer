use std::sync::Arc;

use homer::build;
use homer::build::Compiler as _;

#[test]
fn readme_blocks_pass_checker() -> anyhow::Result<()> {
    let readme_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("README.md");
    let readme = std::fs::read_to_string(&readme_path)?;

    let mut blocks = Vec::new();
    let mut rest = readme.as_str();
    while let Some(start) = rest.find("```rust\n") {
        rest = &rest[start + "```rust\n".len()..];
        let end = rest.find("\n```").unwrap();
        blocks.push(&rest[..end + 1]);
        rest = &rest[end + "\n```".len()..];
    }

    assert!(!blocks.is_empty(), "no ```rust blocks found in README.md");

    let db = &mut build::CompilerDB::new();
    for (i, block) in blocks.into_iter().enumerate() {
        let block = Arc::new(block.to_owned());
        let uri = build::Uri::new(&format!("README.md#block{i}"));
        db.set_input(uri, block.clone());
        let diagnostics =
            db.with_diagnostics(uri, |diags| diags.map(|d| d.layout(&block)).collect::<Vec<_>>());

        assert!(
            diagnostics.is_empty(),
            "README.md code block {i} failed with diagnostics:\n{}",
            diagnostics.join("\n"),
        );
    }
    Ok(())
}

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

    let db = &mut build::CompilerDB::new();
    for path in paths {
        let input = std::fs::read_to_string(&path)?;
        let uri = build::Uri::new(path.to_str().unwrap());
        let input = Arc::new(input);
        db.set_input(uri, input.clone());
        let diagnostics =
            db.with_diagnostics(uri, |diags| diags.map(|d| d.layout(&input)).collect::<Vec<_>>());

        assert!(
            diagnostics.is_empty(),
            "{} failed with diagnostics:\n{}",
            path.display(),
            diagnostics.join("\n"),
        );
    }
    Ok(())
}
