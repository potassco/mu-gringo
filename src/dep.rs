use crate::ast::*;
use petgraph::Graph;
use petgraph::algo::kosaraju_scc;
use std::collections::HashMap;

fn _dep_graph<'a, I: Iterator<Item = &'a Rule>>(prg: I, positive_only: bool) -> Vec<Vec<&'a Rule>> {
    let mut graph = Graph::<&Rule, ()>::new();
    let mut nodes = HashMap::new();
    let mut preds = HashMap::new();
    for rule in prg {
        let idx = graph.add_node(rule);
        let sig = rule.head.sig();
        nodes.insert(rule, idx);
        preds.entry(sig).or_insert(vec!()).push(rule);
    }
    for idx in graph.node_indices() {
        let rule = graph.node_weight(idx).unwrap();
        let hd = nodes.get(rule).unwrap();
        for blit in &rule.body {
            let mut add = |atom: &Atom| {
                if let Some(dep) = preds.get(&atom.sig()) {
                    for bd in dep {
                        graph.add_edge(*hd, *nodes.get(bd).unwrap(), ());
                    }
                }
            };
            match blit {
                BodyLiteral::Literal(Literal::Literal{atom, positive}) if *positive || !positive_only => {
                    add(atom);
                }
                // Note: as in paper but could be refined
                BodyLiteral::Aggregate(aggr) => {
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

pub fn grd_seq<'a, I: Iterator<Item = &'a Rule>>(prg: I) -> Vec<Vec<Vec<&'a Rule>>> {
    let mut ret = vec!();
    for scc in _dep_graph(prg, false) {
        ret.push(vec!());
        for pos_scc in _dep_graph(scc.into_iter(), true) {
            ret.last_mut().unwrap().push(pos_scc);
        }
    }
    return ret;
}
