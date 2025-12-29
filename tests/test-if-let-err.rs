fn if_let_err_simple(result: Result<i32, String>) {
    if let Err(e) = result {
        println!("Error: {}", e);
    }
}

fn if_let_err_scoped(result: Result<i32, String>) {
    if let Result::Err(e) = result {
        println!("Error: {}", e);
    }
}

fn if_let_err_with_else(result: Result<i32, String>) -> i32 {
    if let Err(e) = result {
        println!("Error: {}", e);
        -1
    } else {
        0
    }
}

fn if_let_err_nested(outer: Result<Result<i32, String>, String>) {
    if let Err(e) = outer {
        println!("Outer error: {}", e);
    }
    if let Ok(inner) = outer {
        if let Err(e) = inner {
            println!("Inner error: {}", e);
        }
    }
}
