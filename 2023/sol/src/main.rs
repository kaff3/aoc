use std::env;
use std::fs;

pub mod sols;

fn sanitize(args: &[String]) -> i32 {
    let err_msg = "Please provide a valid solution number to run";

    if args.len() != 2 {
        panic!("{}", err_msg);
    }
    let num: i32 = args[1].trim().parse().expect(err_msg);
    if num < 1 || num > 24 {
        panic!("{}", err_msg);
    }

    num
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let num = sanitize(&args);
    let input = fs::read_to_string("")
        .expect("Could not read input file");
}
