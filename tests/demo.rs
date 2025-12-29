fn some_func() -> i32 {
    info!("Processing result");

    if let Err(e) = fetch_data() {
        warn!("Got error: {}", e);
    }

    match fetch_data() {
        Ok(v) => {
            println!("Success: {}", v);
            v
        }
        Err(e) => {
            error!("Failed: {}", e);
            -1
        }
    }
}


