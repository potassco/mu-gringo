use crate::ast::*;
use std::collections::{HashMap, BTreeSet};
use std::convert::TryInto;

pub type Domain = BTreeSet<Atom>;
type Alpha = (Aggregate, Atom, Vec<String>);
type Eta = (usize, AggregateElement, Vec<String>);

pub struct PropagateState {
    alpha: Vec<Alpha>,
    eta: Vec<Eta>,
    grounding: HashMap<Atom, (Term, BTreeSet<AggregateElement>)>,
}

fn get_idx(args: &Vec<Term>) -> usize {
    assert!(!args.is_empty());
    if let Term::Number(idx) = &args.last().unwrap() {
        (*idx).try_into().unwrap()
    }
    else {
        panic!("must not happen")
    }
}

fn rewrite_aggregate(aggr: &Aggregate, body: Vec<BodyLiteral>, alpha: &mut Vec<Alpha>, eta: &mut Vec<Eta>, rules_eta: &mut Vec<Rule>) -> BodyLiteral {
    // collect the global variables in the aggregate
    let g_vars = {
        let mut set = aggr.get_variables(false);
        aggr.elements.iter().for_each(|elem|
            elem.add_variables(&mut set, false));
        let mut vec: Vec<String> = body.get_variables(false).into_iter().filter(|var|
            set.contains(var)).collect();
        vec.sort();
        vec
    };

    let a_idx = alpha.len();

    // add epsilon rule
    {
        let mut args: Vec<Term> = g_vars.iter().map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(a_idx.try_into().unwrap()));
        let head = Atom{name: "ε".to_string(), args};
        let lhs = Term::Number(0);
        let mut body = body.clone();
        body.push(BodyLiteral::Literal(Literal::Comparison{lhs, rel: aggr.rel, rhs: aggr.guard.clone()}));
        rules_eta.push(Rule{head, body});
    }

    // add eta rules
    for elem in &aggr.elements {
        let e_idx = eta.len();
        // collect local variables in aggregate element
        let l_vars = {
            let mut vec: Vec<String> = elem.get_variables(false).into_iter().filter(|x|
                g_vars.binary_search(x).is_err()).collect();
            vec.sort();
            vec
        };
        let mut args: Vec<Term> = g_vars.iter().chain(l_vars.iter()).map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(e_idx.try_into().unwrap()));

        let head = Atom{name: "η".to_string(), args: args.clone()};
        let mut body = body.clone();
        body.extend(elem.condition.iter().map(|atom|
            BodyLiteral::Literal(Literal::Literal{positive: true, atom: atom.clone()})));

        eta.push((a_idx, elem.clone(), l_vars));
        rules_eta.push(Rule{head, body});
    }

    // replace aggregate with atom
    {
        let mut args: Vec<Term> = g_vars.iter().map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(a_idx.try_into().unwrap()));
        let atom = Atom{name: "α".to_string(), args};
        alpha.push((aggr.clone(), atom.clone(), g_vars));
        BodyLiteral::Literal(Literal::Literal{positive: true, atom})
    }
}

fn rewrite_rule(rule: &Rule, alpha: &mut Vec<Alpha>, eta: &mut Vec<Eta>, rules_eta: &mut Vec<Rule>) -> Rule {
    let body = rule.body.iter().map(|blit|
        if let BodyLiteral::Aggregate(aggr) = blit {
            rewrite_aggregate(aggr, rule.body.iter().filter(|blit|
                matches!(blit, BodyLiteral::Literal(..))).cloned().collect(), alpha, eta, rules_eta)
        }
        else {
            blit.clone()
        }).collect();
    Rule{head: rule.head.clone(), body}
}

pub fn rewrite_component(comp: &Vec<&Rule>) -> (PropagateState, Vec<Rule>, Vec<Rule>) {
    let mut alpha = Vec::new();
    let mut eta = Vec::new();
    let mut rules_e = Vec::new();
    let rules_a = comp.iter().map(|rule|
        rewrite_rule(rule, &mut alpha, &mut eta, &mut rules_e)).collect();
    (PropagateState{alpha, eta, grounding: HashMap::new()}, rules_a, rules_e)
}

fn check_bound(term: &Term, rel: Relation, guard: &Term) -> bool {
    match rel {
        Relation::LessThan => term < guard,
        Relation::LessEqual => term <= guard,
        Relation::GreaterThan => term > guard,
        Relation::GreaterEqual => term >= guard,
        Relation::Equal => term == guard,
        Relation::Inequal => term != guard,
    }
}
fn get_weight(aggr: &Aggregate, elem: &AggregateElement) -> i32 {
    match aggr.fun {
        AggregateFunction::Count => 1,
        AggregateFunction::SumP => {
            if let Some(Term::Number(w)) = elem.terms.first() {
                0.max(*w)
            }
            else {
                0
            }
        },
    }
}

impl PropagateState {
    fn propagate_monotone(&self, aggr: &Aggregate, guard: &Term, elements: &BTreeSet<AggregateElement>) -> bool {
        check_bound(&Term::Number(elements.iter().map(|elem| get_weight(aggr, elem)).sum()), aggr.rel, guard)
    }
    /// Propagate aggregates adding aggregate atoms to the domain if the aggregate became true.
    ///
    /// Maybe this functions needs to know more about he domain.
    pub fn propagate(&mut self, eta: &Vec<Rule>, domain: &mut Domain) {
        let mut todo = BTreeSet::new();
        for rule in eta {
            let Atom{name, args} = &rule.head;
            if name == "ε" {
                let idx_a = get_idx(args);
                let (aggr, aggr_atom, vars) = &self.alpha[idx_a];
                let sub: Substitution = vars.iter().cloned().zip(args.iter().cloned()).collect();
                let aggr_gatom = aggr_atom.apply(&sub);
                let guard = aggr.guard.apply(&sub);
                self.grounding.entry(aggr_gatom.clone()).or_insert((guard, BTreeSet::new()));
                todo.insert(aggr_gatom);
            }
            else if name == "η" {
                let idx_e = get_idx(args);
                let (idx_a, elem, l_vars) = &self.eta[idx_e];
                let (aggr, aggr_atom, vars) = &self.alpha[*idx_a];
                let sub: Substitution = vars.iter().chain(l_vars.iter()).cloned().zip(args.iter().cloned()).collect();
                let aggr_gatom = aggr_atom.apply(&sub);
                let guard = aggr.guard.apply(&sub);
                self.grounding.entry(aggr_gatom.clone()).or_insert((guard, BTreeSet::new())).1.insert(elem.apply(&sub));
                todo.insert(aggr_gatom);
            }
        }
        for gatom in todo {
            let idx_a = get_idx(&gatom.args);
            let (aggr, _, _) = &self.alpha[idx_a];
            let (guard, elements) = self.grounding.get(&gatom).unwrap();
            if match (aggr.fun, aggr.rel) {
                (AggregateFunction::SumP, Relation::GreaterThan) |
                (AggregateFunction::SumP, Relation::GreaterEqual) |
                (AggregateFunction::Count, Relation::GreaterThan) |
                (AggregateFunction::Count, Relation::GreaterEqual) => {
                    self.propagate_monotone(&aggr, guard, elements)
                }
                _ => panic!("Neither `<`, `<=`, `=` nor `!=` is implemented yet. Go implement them!!!")
            } {
                if domain.insert(gatom.clone()) {
                    println!("%         {}", gatom);
                }
            }
        }
    }

    /// Assemble aggregates and replace aggregate atoms in the given vector of rules.
    ///
    /// Furthermore, remove aggregate atoms from the domain.
    pub fn assemble(&self, mut ret: Vec<Rule>, domain: &mut BTreeSet<Atom>) -> Vec<Rule> {
        // Note: retain/drain is still unstable
        let alpha: Vec<Atom> = domain.iter().filter(|atom| atom.name == "α").cloned().collect();
        alpha.iter().for_each(|atom| { domain.remove(atom); });
        for rule in &mut ret {
            for blit in &mut rule.body {
                if let BodyLiteral::Literal(Literal::Literal{atom, positive}) = blit {
                    if atom.name != "α" {
                        continue;
                    }
                    assert!(*positive);
                    let idx_a = get_idx(&atom.args);
                    let (aggr, _, _) = &self.alpha[idx_a];
                    let (guard, elements) = self.grounding.get(atom).unwrap();
                    let aggr = Aggregate{fun: aggr.fun,
                                         rel: aggr.rel,
                                         elements: elements.iter().cloned().collect(),
                                         guard: guard.clone()};
                    *blit = BodyLiteral::Aggregate(aggr);
                }
            }
        }
        ret
    }
}