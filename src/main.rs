#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;
mod grd;
mod prp;

use std::fs;
use std::env;
use std::io::{self, Read};

fn read() -> io::Result<String> {
    let args: Vec<String> = env::args().collect();
    let mut content = String::new();
    for filename in &args[1..] {
        let mut file = fs::File::open(filename)?;
        file.read_to_string(&mut content)?;
    }
    if args.len() == 1 {
        io::stdin().read_to_string(&mut content)?;
    }
    Ok(content)
}

fn main() {
    let content =  read().unwrap();
    let mut prg = ast::parse(&content).unwrap();
    assert!(dep::check(prg.iter()));
    dep::order(&mut prg.iter_mut());
    let seq = dep::grd_seq(prg.iter());
    for rule in grd::ground(&seq) {
        println!("{}", rule);
    }
}
