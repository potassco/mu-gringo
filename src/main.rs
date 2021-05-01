use core::cell::Cell;

#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;
mod grd;

use crate::ast::parse;
use crate::grd::grd;
use crate::dep::{check, grd_seq, order};

fn main() {
    let x = Cell::new(42);
    let y = &x;
    let z = &x;
    y.set(41);
    assert_eq!(z.get(), 41);

    let mut prg = parse("p(1). b(X) :- not a(X), p(X). a(X) :- not b(X), p(X).").unwrap();
    assert!(check(prg.iter()));
    order(&mut prg.iter_mut());
    let seq = grd_seq(prg.iter());
    grd(&seq);
}
