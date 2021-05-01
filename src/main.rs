use core::cell::Cell;

#[macro_use] extern crate lalrpop_util;

mod ast;
mod dep;

use crate::ast::parse;
use crate::dep::{check, dep_graph, order, print_dep_graph};

fn main() {
    let x = Cell::new(42);
    let y = &x;
    let z = &x;
    y.set(41);
    assert_eq!(z.get(), 41);

    let mut prg = parse("p(1). b(X) :- not a(X), p(X). a(X) :- not b(X), p(X).").unwrap();
    assert!(check(prg.iter()));
    order(&mut prg.iter_mut());
    let graph = dep_graph(prg.iter());
    print_dep_graph(&graph);

    // TODO: the graph should be printed!!!
}
