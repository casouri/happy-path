fn simple_err_match(result: Result<i32, String>) -> i32 {
    match result {
        Ok(value) => value,
        Err(e) => {
            println!("Error: {}", e);
            -1
        }
    }
}

fn scoped_err_match(result: Result<i32, String>) -> i32 {
    match result {
        Result::Ok(value) => value,
        Result::Err(e) => {
            println!("Error: {}", e);
            -1
        }
    }
}

fn nested_match(outer: Result<Result<i32, String>, String>) -> i32 {
    match outer {
        Ok(inner) => match inner {
            Ok(value) => value,
            Err(e) => -1,
        },
        Err(e) => -2,
    }
}

fn err_with_guard(result: Result<i32, String>) -> i32 {
    match result {
        Ok(value) if value > 0 => value,
        Ok(_) => 0,
        Err(e) if e.is_empty() => -1,
        Err(_) => -2,
    }
}

fn err_one_liner(result: Result<i32, String>) -> i32 {
    match result {
        Ok(v) => v,
        Err(_) => -1,
    }
}
