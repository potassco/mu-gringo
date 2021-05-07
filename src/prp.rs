use crate::ast::*;
use std::collections::{HashMap, BTreeSet};
use std::convert::TryInto;
use std::cell::Cell;

thread_local! {
    static VERBOSE: Cell<bool> = Cell::new(false);
}

pub fn set_verbose(val: bool) {
    VERBOSE.with(|value| value.set(val));
}

pub fn get_verbose() -> bool {
    VERBOSE.with(|value| value.get())
}

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

impl PropagateState {
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

    fn get_weight(fun: AggregateFunction, elem: &AggregateElement, dom: &Domain) -> i32 {
        if !Self::is_satisfied(dom, &elem.condition) {
            0
        }
        else {
            match fun {
                AggregateFunction::Count => 1,
                AggregateFunction::SumP => {
                    if let Some(Term::Number(w)) = elem.terms.first() {
                        0.max(*w)
                    }
                    else {
                        0
                    }
                },
                AggregateFunction::SumM => {
                    if let Some(Term::Number(w)) = elem.terms.first() {
                        0.min(*w)
                    }
                    else {
                        0
                    }
                },
                AggregateFunction::Sum => {
                    if let Some(Term::Number(w)) = elem.terms.first() {
                        *w
                    }
                    else {
                        0
                    }
                },
            }
        }
    }
    fn is_satisfied(domain: &Domain, condition: &Vec<Atom>) -> bool {
        condition.iter().all(|atom| domain.contains(atom))
    }

    fn propagate_monotone(fun: AggregateFunction, rel: Relation, adjust: i32, guard: &Term, elements: &BTreeSet<AggregateElement>, domain: &Domain) -> bool {
        Self::check_bound(&Term::Number(elements.iter()
                                                .map(|elem| Self::get_weight(fun, elem, domain))
                                                .sum::<i32>() + adjust), rel, guard)
    }

    fn propagate_nonmonotone(rel: Relation, guard: &Term, elements: &BTreeSet<AggregateElement>, dom_i: &Domain, dom_j: &Domain) -> bool {
        let (adjust_fun, propagate_fun) = match rel {
            Relation::LessThan | Relation::LessEqual =>
                (AggregateFunction::SumP, AggregateFunction::SumM),
            Relation::GreaterThan | Relation::GreaterEqual => 
                (AggregateFunction::SumM, AggregateFunction::SumP),
            Relation::Equal | Relation::Inequal => 
                panic!("must not happen")
        };
        let adjust = elements.iter()
                             .map(|elem| Self::get_weight(adjust_fun, elem, dom_i))
                             .sum();
        Self::propagate_monotone(propagate_fun, rel, adjust, guard, elements, dom_j)
    }

    fn propagate_disjunction(elements: &BTreeSet<AggregateElement>, dom_i: &Domain, dom_j: &Domain) -> bool {
        elements.iter().any(|elem| 
            !Self::is_satisfied(dom_i, &elem.condition) && Self::is_satisfied(dom_j, &elem.condition))
    }

    /// Returns true if the sums all subsets of ele_i are inequal to sum_j + guard.
    ///
    /// I made no attempt to implement this efficiently.
    fn check_subsets(ele_i: &[i32], sum_j: i32, guard: &Term) -> bool {
       if ele_i.is_empty() {
           &Term::Number(sum_j) != guard
       }
       else {
           Self::check_subsets(&ele_i[1..], sum_j, guard) && Self::check_subsets(&ele_i[1..], sum_j + ele_i[0], guard)
       }
    }

    fn propagate_inequal(fun: AggregateFunction, guard: &Term, elements: &BTreeSet<AggregateElement>, dom_i: &Domain, dom_j: &Domain) -> bool {
        let sum_j: i32 = elements.iter()
                                 .map(|elem| Self::get_weight(fun, elem, dom_j)).sum();
        let ele_i: Vec<i32> = elements.iter()
                                      .filter(|elem| !Self::is_satisfied(dom_j, &elem.condition))
                                      .map(|elem| Self::get_weight(fun, elem, dom_i))
                                      .filter(|elem| *elem != 0)
                                      .collect();
        Self::check_subsets(&ele_i[..], sum_j, guard)
    }
    /// Propagate aggregates adding aggregate atoms to the domain if the aggregate became true.
    ///
    /// Maybe this functions needs to know more about he domain.
    pub fn propagate(&mut self, eta: &Vec<Rule>, dom_i: &Domain, dom_j: &mut Domain) {
        for rule in eta {
            let Atom{name, args} = &rule.head;
            if name == "ε" {
                let idx_a = get_idx(args);
                let (aggr, aggr_atom, vars) = &self.alpha[idx_a];
                let sub: Substitution = vars.iter().cloned().zip(args.iter().cloned()).collect();
                let aggr_gatom = aggr_atom.apply(&sub);
                let guard = aggr.guard.apply(&sub);
                self.grounding.entry(aggr_gatom.clone()).or_insert((guard, BTreeSet::new()));
            }
            else if name == "η" {
                let idx_e = get_idx(args);
                let (idx_a, elem, l_vars) = &self.eta[idx_e];
                let (aggr, aggr_atom, vars) = &self.alpha[*idx_a];
                let sub: Substitution = vars.iter().chain(l_vars.iter()).cloned().zip(args.iter().cloned()).collect();
                let aggr_gatom = aggr_atom.apply(&sub);
                let guard = aggr.guard.apply(&sub);
                self.grounding.entry(aggr_gatom.clone()).or_insert((guard, BTreeSet::new())).1.insert(elem.apply(&sub));
            }
        }
        for (gatom, (guard, elements)) in self.grounding.iter() {
            if dom_j.contains(gatom) {
                continue;
            }
            let idx_a = get_idx(&gatom.args);
            let (aggr, _, _) = &self.alpha[idx_a];
            if match (aggr.fun, aggr.rel) {
                (AggregateFunction::Count, Relation::GreaterThan) |
                (AggregateFunction::Count, Relation::GreaterEqual) |
                (AggregateFunction::SumP, Relation::GreaterThan) |
                (AggregateFunction::SumP, Relation::GreaterEqual) |
                (AggregateFunction::SumM, Relation::LessThan) |
                (AggregateFunction::SumM, Relation::LessEqual) =>
                    Self::propagate_monotone(aggr.fun, aggr.rel, 0, guard, elements, dom_j),
                (AggregateFunction::Count, Relation::LessThan) |
                (AggregateFunction::Count, Relation::LessEqual) |
                (AggregateFunction::SumP, Relation::LessThan) |
                (AggregateFunction::SumP, Relation::LessEqual) |
                (AggregateFunction::SumM, Relation::GreaterThan) |
                (AggregateFunction::SumM, Relation::GreaterEqual) =>
                    Self::propagate_monotone(aggr.fun, aggr.rel, 0, guard, elements, dom_i),
                (AggregateFunction::Count, Relation::Inequal) =>
                    Self::propagate_monotone(aggr.fun, Relation::GreaterThan, 0, guard, elements, dom_j) ||
                        Self::propagate_monotone(aggr.fun, Relation::LessThan, 0, guard, elements, dom_i) ||
                        Self::propagate_disjunction(elements, dom_i, dom_j),
                (AggregateFunction::Count, Relation::Equal) =>
                    Self::propagate_monotone(aggr.fun, Relation::GreaterEqual, 0, guard, elements, dom_j) &&
                        Self::propagate_monotone(aggr.fun, Relation::LessEqual, 0, guard, elements, dom_i),
                (AggregateFunction::SumP, Relation::Inequal) =>
                    Self::propagate_monotone(aggr.fun, Relation::GreaterThan, 0, guard, elements, dom_j) ||
                        Self::propagate_monotone(aggr.fun, Relation::LessThan, 0, guard, elements, dom_i) ||
                        Self::propagate_inequal(aggr.fun, &guard, &elements, dom_i, dom_j),
                (AggregateFunction::SumP, Relation::Equal) =>
                    Self::propagate_monotone(aggr.fun, Relation::GreaterEqual, 0, guard, elements, dom_j) &&
                        Self::propagate_monotone(aggr.fun, Relation::LessEqual, 0, guard, elements, dom_i) &&
                        !Self::propagate_inequal(aggr.fun, &guard, &elements, dom_j, dom_i),
                (AggregateFunction::SumM, Relation::Inequal) =>
                    Self::propagate_monotone(aggr.fun, Relation::LessThan, 0, guard, elements, dom_j) ||
                        Self::propagate_monotone(aggr.fun, Relation::GreaterThan, 0, guard, elements, dom_i) ||
                        Self::propagate_inequal(aggr.fun, &guard, &elements, dom_i, dom_j),
                (AggregateFunction::SumM, Relation::Equal) =>
                    Self::propagate_monotone(aggr.fun, Relation::LessEqual, 0, guard, elements, dom_j) &&
                        Self::propagate_monotone(aggr.fun, Relation::GreaterEqual, 0, guard, elements, dom_i) &&
                        !Self::propagate_inequal(aggr.fun, &guard, &elements, dom_j, dom_i),
                (AggregateFunction::Sum, Relation::GreaterThan) |
                (AggregateFunction::Sum, Relation::GreaterEqual) |
                (AggregateFunction::Sum, Relation::LessThan) |
                (AggregateFunction::Sum, Relation::LessEqual) =>
                    Self::propagate_nonmonotone(aggr.rel, guard, elements, dom_i, dom_j),
                (AggregateFunction::Sum, _) =>
                    panic!("implement sum with = and !=")
            } {
                if get_verbose() {
                    println!("%         {}", gatom);
                }
                dom_j.insert(gatom.clone());
            }
        }
    }

    /// Assemble aggregates and replace aggregate atoms in the given vector of rules.
    ///
    /// Furthermore, remove aggregate atoms from the domain.
    pub fn assemble(&self, mut ret: Vec<Rule>, domain: &mut BTreeSet<Atom>) -> Vec<Rule> {
        self.grounding.iter().for_each(|(atom, _)| { domain.remove(atom); });
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
