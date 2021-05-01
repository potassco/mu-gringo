use crate::ast::*;

pub fn print_dep_graph<'a>(graph: &'a Vec<Vec<Vec<&'a Rule>>>) {
    for scc in graph {
        println!("Component");
        for ref_scc in scc {
            println!("  Refined Component");
            for rule in ref_scc {
                println!("    {}.\n", rule);
            }
        }
    }
}

pub fn grd<'a>(seq: &'a Vec<Vec<Vec<&'a Rule>>>) {
    use core::cell::Cell;

    let x = Cell::new(42);
    let y = &x;
    let z = &x;
    y.set(41);
    assert_eq!(z.get(), 41);

    print_dep_graph(&seq);
    // - ground rule w.r.t. sets I and J
    // - ground component w.r.t. sets I and J
    // - ground program
}
