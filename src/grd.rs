use crate::ast::*;
use std::collections::{BTreeSet, HashMap};

fn print_dep_graph<'a>(graph: &'a Vec<Vec<Vec<&'a Rule>>>) {
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

struct Domain {
    atoms_old: BTreeSet<Atom>,
    atoms_all: BTreeSet<Atom>,
}

trait GroundMatch {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool;
}

impl GroundMatch for Term {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool {
        match (self, g) {
            (Term::Number(x), Term::Number(y)) => x == y,
            (Term::Variable(x), _) => {
                if let Some(t) = s.mapping.get(x) {
                    t == g
                }
                else {
                    s.mapping.insert(x.clone(), g.clone());
                    true
                }
            },
            (Term::Function(name, args), Term::Function(name_g, args_g)) if name == name_g && args.len() == args_g.len() => {
                args.iter().zip(args_g).all(|(u, v)| u.ground_match(v, s))
            },
            _ => false
        }
    }
}

impl GroundMatch for Atom {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool {
        self.name == g.name && self.args.len() == g.args.len() && self.args.iter().zip(&g.args).all(|(u, v)| u.ground_match(v, s))
    }
}

// Note: certainly one of the most inefficient ways to implement this
fn matches(s: &Substitution, dom_i: &Domain, dom_j: &Domain, lit: &Literal) -> Vec<(Substitution, bool)> {
    let mut ret: Vec<(Substitution, bool)> = Vec::new();
    match lit {
        Literal::Literal{positive: true, atom} => {
            let a = atom.apply(s);
            for g in &dom_j.atoms_all {
                let mut sp = Substitution{mapping: HashMap::new()};
                a.ground_match(&g, &mut sp);
                sp.mapping.extend(s.mapping.iter().map(|(u, v)| (u.clone(), v.clone())));
                ret.push((sp, !dom_j.atoms_old.contains(g)));
            }

        },
        Literal::Literal{positive: false, atom} => {
            let g = atom.apply(s);
            if !dom_i.atoms_all.contains(&g) {
                ret.push((s.clone(), false));
            }
        },
        Literal::Comparison{lhs, rel, rhs} => {
            let l = lhs.apply(s);
            let r = rhs.apply(s);
            match rel {
                Relation::LessThan if l < r => { ret.push((s.clone(), false)); },
                Relation::LessEqual if l <= r => { ret.push((s.clone(), false)); },
                Relation::GreaterThan if l > r => { ret.push((s.clone(), false)); },
                Relation::GreaterEqual if l >= r => { ret.push((s.clone(), false)); },
                _ => { },
            }
        },
    };
    ret
}

fn ground_rule(s: &Substitution, dom_i: &Domain, dom_j: &Domain, rule: &Rule, i: usize, new: bool) -> Vec<Rule> {
    let mut ret = Vec::new();
    if i < rule.body.len() {
        let lit = match &rule.body[i] {
            BodyLiteral::Literal(lit) => lit,
            BodyLiteral::Aggregate(..) => panic!("must not happen!"),
        };
        for (sp, newp) in matches(s, dom_i, dom_j, &lit) {
            ret.extend(ground_rule(&sp, dom_i, dom_j, rule, i + 1, new || newp))
        }
    }
    else if dom_j.atoms_old.is_empty() || new {
        ret.push(rule.apply(s))
    }
    ret
}

fn ground_component(dom_i: &Domain, dom_j: &Domain, rule: &Vec<&Rule>) -> Vec<Rule> {
    let mut ret = Vec::new();

    ret
}

pub fn ground<'a>(seq: &'a Vec<Vec<Vec<&'a Rule>>>) {
    use core::cell::Cell;

    let x = Cell::new(42);
    let y = &x;
    let z = &x;
    y.set(41);
    assert_eq!(z.get(), 41);

    print_dep_graph(&seq);
    // Domain:
    // - I and J create indices on the fly
    // - ground rule w.r.t. sets I and J
    // - ground component w.r.t. sets I and J
    // - ground program
}

