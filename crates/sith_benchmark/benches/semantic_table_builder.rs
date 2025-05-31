use std::path::Path;
use std::sync::{Arc, RwLock};

use ruff_python_resolver::cache::ImportResolverCache;
use ruff_python_resolver::config::Config;
use ruff_python_resolver::execution_environment::ExecutionEnvironment;
use sith_benchmark::criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkId, Criterion, Throughput,
};
use sith_benchmark::{TestCase, TestFile, TestFileDownloadError, TARGET_DIR};
use sith_python_parser::parse_module;
use sith_python_utils::interpreter::resolve_python_interpreter;
use sith_python_utils::PythonHost;
use sith_semantic_model::indexer::FileId;
use sith_semantic_model::symbol_table::{ImportResolverConfig, SymbolTableBuilder};

const TOMLLIB_312_URL: &str = "https://raw.githubusercontent.com/python/cpython/8e8a4baf652f6e1cee7acde9d78c4b6154539748/Lib/tomllib";

fn create_test_cases() -> Result<Vec<TestCase>, TestFileDownloadError> {
    Ok(vec![
        TestCase::fast(get_test_file("__init__.py")?),
        TestCase::fast(get_test_file("_types.py")?),
        TestCase::normal(get_test_file("_parser.py")?),
        TestCase::normal(get_test_file("_re.py")?),
        // TestCase::slow(TestFile::try_download(
        //     "large/dataset.py",
        //     "https://raw.githubusercontent.com/DHI/mikeio/b7d26418f4db2909b0aa965253dbe83194d7bb5b/tests/test_dataset.py",
        // )?),
    ])
}

fn get_test_file(name: &str) -> Result<TestFile, TestFileDownloadError> {
    let path = format!("tomllib/{name}");
    let url = format!("{TOMLLIB_312_URL}/{name}");
    TestFile::try_download(&path, &url)
}

fn benchmark_semantic_table_builder(criterion: &mut Criterion<WallTime>) {
    let test_cases = create_test_cases().unwrap();
    let mut group = criterion.benchmark_group("semantic_builder");
    let root = TARGET_DIR.join("tomllib");
    let interpreter =
        resolve_python_interpreter(&root).expect("Failed to find valid python interpreter!");
    let python_host = PythonHost::new(&interpreter);
    let exec_env = ExecutionEnvironment {
        root,
        python_version: python_host.version,
        python_platform: python_host.platform,
        extra_paths: vec![],
    };
    let config = Config {
        typeshed_path: Some(sith_vendored::setup_typeshed()),
        stub_path: None,
        venv_path: None,
        venv: None,
    };
    let import_resolver_cache = Arc::new(RwLock::new(ImportResolverCache::default()));
    for case in test_cases {
        let parsed = parse_module(case.code());

        group.throughput(Throughput::Bytes(case.code().len() as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(case.name()),
            &case,
            |b, case| {
                b.iter(|| {
                    let resolver_cfg = ImportResolverConfig::new(&exec_env, &config, &python_host);
                    let table_builder = SymbolTableBuilder::new(
                        Path::new(case.name()),
                        FileId::from_u32(0),
                        resolver_cfg,
                        import_resolver_cache.clone(),
                    );
                    table_builder.build(parsed.suite());
                });
            },
        );
    }

    group.finish();
}

criterion_group!(semantic_builder, benchmark_semantic_table_builder);
criterion_main!(semantic_builder);
