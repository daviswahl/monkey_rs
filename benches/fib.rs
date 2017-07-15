#![feature(test)]

extern crate test;

extern crate monkey_parser;

fn fibonacci(x: i64) -> i64 {
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            1
        } else {
            fibonacci(x - 1) + fibonacci(x - 2)
        }
    }

}

fn test_fib() -> String {
    let input = "
    let fibonacci = fn(x) {
        if (x == 0) {
            0
        } else {
            if (x == 1) {
                1
            } else {
                fibonacci(x - 1) + fibonacci(x - 2);
            }
        }
    };
    fibonacci(36);
    ";

    use monkey_parser;
    match monkey_parser::run(input) {
        Ok(f) => f.to_string(),
        Err(e) => e.to_string()
    }
}

fn test_fib_native() -> i64 {
    fibonacci(20)
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_fib(b: &mut Bencher) {
       b.iter(|| test_fib())
    }

    #[bench]
    fn bench_fib_native(b: &mut Bencher) {
        b.iter( || {
            let n = test::black_box(1000);
            test_fib_native()
        })
    }
}