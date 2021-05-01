use crate::ast::*;
use petgraph::Graph;
use petgraph::algo::kosaraju_scc;
use std::collections::HashMap;

fn get_sig(atom: &Atom) -> (&str, usize) {
    return (&atom.name, atom.args.len());
}

fn is_positive(aggr: &Aggregate) -> bool {
    match (&aggr.fun, &aggr.rel) {
        (AggregateFunction::Count, Relation::GreaterThan) => true,
        (AggregateFunction::Count, Relation::GreaterEqual) => true,
        _ => false,
    }
}

fn _dep_graph<'a, I: Iterator<Item = &'a Rule>>(prg: I, positive_only: bool) -> Vec<Vec<&'a Rule>> {
    let mut graph = Graph::<&Rule, ()>::new();
    let mut nodes = HashMap::new();
    let mut preds = HashMap::new();
    for rule in prg {
        let idx = graph.add_node(rule);
        let sig = get_sig(&rule.head);
        nodes.insert(rule, idx);
        preds.insert(sig, vec!());
        preds.get_mut(&sig).unwrap().push(rule);
    }
    for idx in graph.node_indices() {
        let rule = graph.node_weight(idx).unwrap();
        let hd = nodes.get(rule).unwrap();
        for blit in &rule.body {
            let mut add = |atom| {
                if let Some(dep) = preds.get(&get_sig(atom)) {
                    for bd in dep {
                        graph.add_edge(*hd, *nodes.get(bd).unwrap(), ());
                    }
                }
            };
            match blit {
                BodyLiteral::Literal(Literal::Literal{atom, positive}) if *positive || !positive_only => {
                    add(atom);
                }
                BodyLiteral::Aggregate(aggr) => if is_positive(aggr) || !positive_only {
                    for elem in &aggr.elements {
                        for atom in &elem.condition {
                            add(atom);
                        }
                    }
                }
                _ => ()
            }
        }
    }
    let mut sccs = vec!();
    for scc in kosaraju_scc(&graph) {
        sccs.push(vec!());
        for idx in scc {
            sccs.last_mut().unwrap().push(*graph.node_weight(idx).unwrap());
        }
    }
    return sccs;
}

pub fn check<'a, I: Iterator<Item = &'a Rule>>(prg: I) -> bool {
    for rule in prg {
        let vars = rule.get_variables(false);
        let bound = rule.get_variables(true);
        if !vars.is_subset(&bound) {
            return false;
        }
        for lit in &rule.body {
            if let BodyLiteral::Aggregate(aggr) = lit {
                for elem in &aggr.elements {
                    let vars = elem.get_variables(false);
                    let bound = elem.condition.get_variables(true);
                    if !vars.is_subset(&bound) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

pub fn order<'a, I: Iterator<Item = &'a mut Rule>>(prg: I) {
    for rule in prg {
        rule.body.sort_by_key(|lit| match lit {
            BodyLiteral::Literal(Literal::Literal{ positive: true, .. }) => 0,
            _ => 1,
        });
    }
}

pub fn dep_graph<'a, I: Iterator<Item = &'a Rule>>(prg: I) -> Vec<Vec<Vec<&'a Rule>>> {
    let mut ret = vec!();
    for scc in _dep_graph(prg, false) {
        ret.push(vec!());
        for pos_scc in _dep_graph(scc.into_iter(), true) {
            ret.last_mut().unwrap().push(pos_scc);
        }
    }
    return ret;
}

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
