#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;
mod grd;
mod prp;

use std::fs;
use std::io::{self, Read};
use clap::Parser;
use prp::set_verbose;

fn read(files: Vec<String>) -> io::Result<String> {
    let mut content = String::new();
    if !files.is_empty() {
        for filename in files {
            let mut file = fs::File::open(filename)?;
            file.read_to_string(&mut content)?;
        }
    }
    else {
        io::stdin().read_to_string(&mut content)?;
    }
    Ok(content)
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    files: Vec<String>,

    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();
    let content = read(args.files).unwrap();
    set_verbose(args.verbose);

    let mut prg = ast::parse(&content).unwrap();
    assert!(dep::check(prg.iter()));
    dep::order(&mut prg.iter_mut());
    let seq = dep::grd_seq(prg.iter());
    for rule in grd::ground(&seq) {
        println!("{}", rule);
    }
}
