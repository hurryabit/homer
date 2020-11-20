use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use homer_compiler::*;
use std::sync::Arc;
use std::time::Duration;

const ADDRESS_VAR: &str = "HOMER_BENCH";

pub fn criterion_benchmark(c: &mut Criterion) {
    use std::env::VarError;
    let address = match std::env::var(ADDRESS_VAR) {
        Ok(val) => val,
        Err(VarError::NotPresent) => String::from("../examples/bench.doh:main"),
        Err(VarError::NotUnicode(_)) => panic!(
            "Environment variable {} contains non-unicode characters.",
            ADDRESS_VAR,
        ),
    };
    let address_parts: Vec<&str> = address.split(':').collect();
    if address_parts.len() != 2 {
        panic!("scenario address needs to be of the form 'path/to/homer-file.doh:function_name'");
    }
    let path = address_parts[0];
    let file = path.split('/').last().unwrap();
    let main = address_parts[1];

    let db = &mut build::CompilerDB::new();
    let uri = build::Uri::new("bench.doh");
    let input = Arc::new(std::fs::read_to_string(std::path::Path::new(path)).unwrap());
    db.set_input(uri, Arc::clone(&input));
    let mut success = true;
    db.with_diagnostics(uri, |diagnostics| {
        for diagnostic in diagnostics {
            success = false;
            eprintln!("{}\n{}", "-".repeat(50), diagnostic.layout(&input));
        }
    });
    if !success {
        panic!("Loading the benchmark function failed.");
    }
    let module = db.anf_module(uri).unwrap();
    let machine = cek::Machine::new(&module, homer_compiler::syntax::ExprVar::new(main));

    c.bench_with_input(
        BenchmarkId::new("run-cek", format!("{}:{}", file, main)),
        &machine,
        |b, machine| {
            b.iter(|| {
                machine.clone().run();
            })
        },
    );
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .measurement_time(Duration::from_secs(30))
        .warm_up_time(Duration::from_secs(10))
        .sample_size(20)
        .configure_from_args();
    targets = criterion_benchmark
}
criterion_main!(benches);
