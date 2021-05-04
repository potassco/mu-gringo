use crate::ast::*;
use std::collections::BTreeSet;
use std::convert::TryInto;

pub type Domain = BTreeSet<Atom>;
type Alpha = (Aggregate, Vec<String>);
type Eta = (usize, AggregateElement, Vec<String>);

pub struct PropagateState {
    alpha: Vec<Alpha>,
    eta: Vec<Eta>,
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
    alpha.push((aggr.clone(), g_vars));

    // add epsilon rule
    {
        let mut args: Vec<Term> = alpha[a_idx].1.iter().map(|var|
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
                alpha[a_idx].1.binary_search(x).is_err()).collect();
            vec.sort();
            vec
        };
        let mut args: Vec<Term> = alpha[a_idx].1.iter().chain(l_vars.iter()).map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(e_idx.try_into().unwrap()));

        let head = Atom{name: "η".to_string(), args: args.clone()};
        let mut body = body.clone();
        body.extend(elem.condition.iter().map(|atom|
            BodyLiteral::Literal(Literal::Literal{positive: true, atom: atom.clone()})));

        eta.push((e_idx, elem.clone(), l_vars));
        rules_eta.push(Rule{head, body});
    }

    // replace aggregate with atom
    {
        let mut args: Vec<Term> = alpha[a_idx].1.iter().map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(a_idx.try_into().unwrap()));
        let atom = Atom{name: "α".to_string(), args};
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
    (PropagateState{alpha, eta}, rules_a, rules_e)
}

impl PropagateState {
    /// Propagate aggregates adding aggregate atoms to the domain if the aggregate became true.
    ///
    /// Maybe this functions needs to know more about he domain.
    pub fn propagate(&mut self, eta: &Vec<Rule>, domain: &mut BTreeSet<Atom>) {
        for rule in eta {
            let Atom{name, args} = &rule.head;
            if name == "ε" {
                panic!("Implement me! - Please!!!");
            }
            else if name == "η" {
                let idx_e = get_idx(args);
                let (idx_a, elem, l_vars) = &self.eta[idx_e];
                let (a, vars) = &self.alpha[*idx_a];
                let e = &a.elements[idx_e];
                let g_sub: Substitution = vars.iter().cloned().zip(args.iter().cloned()).collect();
                let l_sub: Substitution = vars.iter().chain(l_vars.iter()).cloned().zip(args.iter().cloned()).collect();
                println!("gathered    : {}", rule.head);
                println!("  accumulate: {}", e.apply(&l_sub));
                println!("  propagate : {}", a.apply(&g_sub));
            }
        }
    }

    /// Assemble aggregates and replace aggregate atoms in the given vector of rules.
    ///
    /// Furthermore, remove aggregate atoms from the domain.
    pub fn assemble(&self, ret: Vec<Rule>, domain: &mut BTreeSet<Atom>) -> Vec<Rule> {
        ret
    }
}
