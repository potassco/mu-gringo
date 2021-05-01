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
    print_dep_graph(&seq);
    // - ground rule w.r.t. sets I and J
    // - ground component w.r.t. sets I and J
    // - ground program
    
}
