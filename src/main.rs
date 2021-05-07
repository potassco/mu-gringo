#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;
mod grd;
mod prp;

use std::fs;
use std::io::{self, Read};
use clap::{Arg, App, Values};
use prp::set_verbose;

fn read(ofiles: Option<Values>) -> io::Result<String> {
    let mut content = String::new();
    if let Some(files) = ofiles {
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

fn main() {
    let matches = App::new("grounding")
        .version("1.0")
        .about("Educational algorithms showing how to ground logic programs")
        .arg(Arg::with_name("files")
            .help("Files with logic programs")
            .index(1)
            .multiple(true))
        .arg(Arg::with_name("verbose")
            .short("v")
            .long("verbose")
            .help("Enable verbose output"))
        .get_matches();

    let content = read(matches.values_of("files")).unwrap();
    set_verbose(matches.occurrences_of("verbose") > 0);

    let mut prg = ast::parse(&content).unwrap();
    assert!(dep::check(prg.iter()));
    dep::order(&mut prg.iter_mut());
    let seq = dep::grd_seq(prg.iter());
    for rule in grd::ground(&seq) {
        println!("{}", rule);
    }
}
