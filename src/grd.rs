use crate::ast::*;
use crate::prp::*;
use std::collections::HashMap;
use std::convert::TryInto;

trait GroundMatch {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool;
}

impl GroundMatch for Term {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool {
        match (self, g) {
            (Term::Number(x), Term::Number(y)) =>
                x == y,
            (Term::Variable(x), _) =>
                if let Some(t) = s.get(x) {
                    t == g
                }
                else {
                    s.insert(x.clone(), g.clone());
                    true
                },
            (Term::Function(name, args), Term::Function(name_g, args_g)) =>
                name == name_g && args.len() == args_g.len() && 
                    args.iter().zip(args_g).all(|(u, v)| u.ground_match(v, s)),
            _ => 
                false,
        }
    }
}

impl GroundMatch for Atom {
    fn ground_match(&self, g: &Self, s: &mut Substitution) -> bool {
        self.name == g.name &&
            self.args.len() == g.args.len() && 
            self.args.iter().zip(&g.args).all(|(u, v)| u.ground_match(v, s))
    }
}

// Note: certainly one of the most inefficient ways to implement this
fn matches(s: &Substitution, dom_i: &Domain, dom_j: &Domain, dom_jp: &Domain, lit: &Literal) -> Vec<(Substitution, bool)> {
    let mut ret: Vec<(Substitution, bool)> = Vec::new();
    match lit {
        Literal::Literal{positive: true, atom} => {
            let a = atom.apply(s);
            for g in dom_j {
                let mut sp = Substitution::new();
                if a.ground_match(&g, &mut sp) {
                    sp.extend(s.iter().map(|(u, v)| (u.clone(), v.clone())));
                    ret.push((sp, !dom_jp.contains(g)));
                }
            }

        },
        Literal::Literal{positive: false, atom} => {
            let g = atom.apply(s);
            if !dom_i.contains(&g) {
                ret.push((s.clone(), false));
            }
        },
        Literal::Comparison{lhs, rel, rhs} => {
            let l = lhs.apply(s);
            let r = rhs.apply(s);
            if match rel {
                Relation::LessThan => l < r,
                Relation::LessEqual => l <= r,
                Relation::GreaterThan => l > r,
                Relation::GreaterEqual => l >= r,
                Relation::Equal => l == r,
                Relation::Inequal => l != r,
            } {
                ret.push((s.clone(), false));
            }
        },
    };
    ret
}

fn ground_rule(s: &Substitution, dom_i: &Domain, dom_j: &Domain, dom_jp: &Domain, rule: &Rule, i: usize, new: bool) -> Vec<Rule> {
    let mut ret = Vec::new();
    if i < rule.body.len() {
        let lit = match &rule.body[i] {
            BodyLiteral::Literal(lit) => lit,
            BodyLiteral::Aggregate(..) => panic!("must not happen!"),
        };
        for (sp, newp) in matches(s, dom_i, dom_j, dom_jp, &lit) {
            ret.extend(ground_rule(&sp, dom_i, dom_j, dom_jp, rule, i + 1, new || newp))
        }
    }
    else if dom_jp.is_empty() || new {
        ret.push(rule.apply(s))
    }
    ret
}

fn rewrite_aggregate(aggr: &Aggregate, body: Vec<BodyLiteral>, alpha: &mut Vec<(Aggregate, Vec<String>)>, eta: &mut Vec<Rule>) -> BodyLiteral {
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
        eta.push(Rule{head, body});
    }

    // add eta rules
    for (e_idx, elem) in aggr.elements.iter().enumerate() {
        let mut args: Vec<Term> = alpha[a_idx].1.iter().map(|var|
            Term::Variable(var.clone())).collect();
        args.push(Term::Number(a_idx.try_into().unwrap()));
        args.push(Term::Number(e_idx.try_into().unwrap()));

        let head = Atom{name: "η".to_string(), args: args.clone()};
        let mut body = body.clone();
        body.extend(elem.condition.iter().map(|atom|
            BodyLiteral::Literal(Literal::Literal{positive: true, atom: atom.clone()})));
        eta.push(Rule{head, body});
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

fn rewrite_rule(rule: &Rule, alpha: &mut Vec<(Aggregate, Vec<String>)>) -> (Rule, Vec<Rule>) {
    let mut eta = Vec::new();
    let body = rule.body.iter().map(|blit|
        if let BodyLiteral::Aggregate(aggr) = blit {
            rewrite_aggregate(aggr, rule.body.iter().filter(|blit| 
                matches!(blit, BodyLiteral::Literal(..))).cloned().collect(), alpha, &mut eta)
        }
        else {
            blit.clone()
        }).collect();
    (Rule{head: rule.head.clone(), body}, eta)
}

fn rewrite_component(comp: &Vec<&Rule>) -> (Vec<(Aggregate, Vec<String>)>, Vec<(Rule, Vec<Rule>)>) {
    let mut alpha = Vec::new();
    let compr = comp.iter().map(|rule| {
        let (ruler, eta) = rewrite_rule(rule, &mut alpha);
        (ruler, eta)
        }).collect();
    (alpha, compr)
}

fn ground_component(alpham: &Vec<(Aggregate, Vec<String>)>, dom_i: &Domain, dom_j: &mut Domain, alpha: &Vec<&Rule>, eta: &Vec<&Rule>) -> Vec<Rule> {
    let mut ret = Vec::new();
    let mut dom_jp = Domain::new();
    let mut alphas = PropagateState::new(alpham);
    loop {
        // ground eta/epsilon rules
        let mut eta_g = Vec::new();
        for rule in eta {
            println!("%       {}", rule);
            eta_g.append(&mut ground_rule(&Substitution::new(), dom_i, dom_j, &dom_jp, rule, 0, false));
        }
        println!("ground eta_g: {}", eta_g.len());
        // propagate aggregates
        alphas.propagate(&eta_g, &mut dom_jp);
        // ground aggregate rules
        let m = ret.len();
        for rule in alpha {
            println!("%       {}", rule);
            ret.append(&mut ground_rule(&Substitution::new(), dom_i, dom_j, &dom_jp, rule, 0, false));
        }
        // next generation
        dom_jp = dom_j.clone();
        for rule in &ret[m..] {
            println!("%         {}", rule);
            dom_j.insert(rule.head.clone());
        }
        if dom_jp.len() == dom_j.len() {
            break;
        }
    }
    alphas.assemble(ret, dom_j)
}

/// Returns true for rules that do not have open negative occurences of predicates.
///
/// This does not mean that the rule belongs to a stratified component but just that we can use it
/// to derive facts from facts seen previously.
fn is_stratified(rule: &Rule, open: &HashMap<(&str, usize), i32>) -> bool {
    rule.body.iter().all(|blit| match blit {
        BodyLiteral::Literal(Literal::Literal{positive: false, atom}) =>
            *open.get(&atom.sig()).unwrap_or(&0) == 0,
        BodyLiteral::Aggregate(aggr) if aggr.has_negative() =>
            aggr.elements.iter().all(
                |elem| elem.condition.iter().all(
                    |atom| *open.get(&atom.sig()).unwrap_or(&0) == 0)),
        _ =>
            true,
    })
}

pub fn ground(seq: &Vec<Vec<Vec<&Rule>>>) -> Vec<Rule> {
    let mut dom_i = Domain::new();
    let mut dom_j = Domain::new();
    let mut f = Vec::new();
    let mut g = Vec::new();
    for comp in seq {
        println!("% Ground Component");
        let mut open = HashMap::new();
        for ref_comp in comp {
            ref_comp.iter().for_each(|rule| *open.entry(rule.head.sig()).or_insert(0) += 1);
        }
        for ref_comp in comp {
            let (alpham, ref_compr) = rewrite_component(ref_comp);
            let mut alphap: Vec<&Rule> = Vec::new();
            let mut alpha: Vec<&Rule> = Vec::new();
            let mut etap: Vec<&Rule> = Vec::new();
            let mut eta: Vec<&Rule> = Vec::new();
            for (rule, (rule_a, rules_e)) in ref_comp.iter().zip(ref_compr.iter()) {
                if is_stratified(rule, &open) {
                    alphap.push(&rule_a);
                    etap.extend(rules_e.iter());
                }
                alpha.push(&rule_a);
                eta.extend(rules_e.iter());
            }

            println!("%   Ground Refined Component");
            println!("%     Ground Facts");
            f.extend(ground_component(&alpham, &dom_j, &mut dom_i, &alphap, &etap));
            println!("%     Ground Program");
            g.extend(ground_component(&alpham, &dom_i, &mut dom_j, &alpha, &eta));

            ref_comp.iter().for_each(|rule| *open.get_mut(&rule.head.sig()).unwrap() -= 1);
        }
    }
    g
}

