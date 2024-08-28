use std::num::NonZeroUsize;

use tracing_subscriber::{layer::SubscriberExt, Registry};
use tracing_tree::time::Uptime;

fn main() -> anyhow::Result<()> {
    let subscriber = Registry::default().with(
        tracing_tree::HierarchicalLayer::default()
            .with_indent_lines(true)
            .with_indent_amount(2)
            .with_bracketed_fields(true)
            .with_targets(true)
            .with_writer(|| Box::new(std::io::stderr()))
            .with_timer(Uptime::default()), // .with_filter(LoggingFilter { trace_level }),
    );

    tracing::subscriber::set_global_default(subscriber)?;

    let max_cpu_count = NonZeroUsize::new(4).unwrap();

    let worker_threads = std::thread::available_parallelism()
        .unwrap_or(max_cpu_count)
        .max(max_cpu_count);
    let server = sith_server::Server::new(
        NonZeroUsize::try_from(worker_threads).expect("a non-zero worker thread count"),
    )?;

    server.run()
}
