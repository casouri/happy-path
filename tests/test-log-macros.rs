use log::{info, warn, debug, error, trace};

fn test_simple_macros() {
    info!("This is info");
    warn!("This is warn");
    debug!("This is debug");
    error!("This is error");
    trace!("This is trace");
}

fn test_scoped_macros() {
    log::info!("Scoped info");
    log::warn!("Scoped warn");
    tracing::debug!("Tracing debug");
    tracing::error!("Tracing error");
}

fn test_with_args() {
    let x = 42;
    info!("Value: {}", x);
    error!("Error code: {}", x);
}

fn test_println_dbg() {
    let x = 42;
    println!("Hello world");
    println!("Value: {}", x);
    dbg!(x);
    dbg!(x + 1);
}
