#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;
mod grd;

use crate::ast::parse;
use crate::grd::grd;
use crate::dep::{check, grd_seq, order};

fn main() {
    let mut prg = parse("p(1). b(X) :- not a(X), p(X). a(X) :- not b(X), p(X).").unwrap();
    assert!(check(prg.iter()));
    order(&mut prg.iter_mut());
    let seq = grd_seq(prg.iter());
    grd(&seq);
}
