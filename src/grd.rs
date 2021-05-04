use crate::ast::*;
use crate::prp::*;
use std::collections::HashMap;

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
    else if new {
        ret.push(rule.apply(s));
        println!("%           {}", ret.last().unwrap());
    }
    ret
}

fn ground_component(dom_i: &Domain, dom_j: &mut Domain, comp: &Vec<&Rule>) -> Vec<Rule> {
    let mut ret = Vec::new();
    let mut dom_jp = Domain::new();
    let mut new = true; // Note: this addresses a glitch in the paper
    let (mut alphas, alpha, eta) = rewrite_component(comp);

    loop {
        if !eta.is_empty() {
            // ground eta/epsilon rules
            println!("%       Ground Element Rules");
            let mut eta_g = Vec::new();
            for rule in &eta {
                println!("%         {}", rule);
                eta_g.append(&mut ground_rule(&Substitution::new(), dom_i, dom_j, &dom_jp, &rule, 0, new));
            }
            // propagate aggregates
            println!("%       Propagate Aggregates");
            alphas.propagate(&eta_g, dom_j);
        }
        // ground aggregate rules
        println!("%       Ground Rules");
        let m = ret.len();
        for rule in &alpha {
            println!("%         {}", rule);
            ret.append(&mut ground_rule(&Substitution::new(), dom_i, dom_j, &dom_jp, &rule, 0, new));
        }
        // next generation
        dom_jp = dom_j.clone();
        for rule in &ret[m..] {
            dom_j.insert(rule.head.clone());
        }
        new = false;
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
            let ref_compp = ref_comp.iter().filter(|rule| is_stratified(rule, &open)).cloned().collect();

            println!("%   Ground Refined Component");
            println!("%     Ground Certain");
            f.extend(ground_component(&dom_j, &mut dom_i, &ref_compp));
            println!("%     Ground Possible");
            g.extend(ground_component(&dom_i, &mut dom_j, &ref_comp));

            ref_comp.iter().for_each(|rule| *open.get_mut(&rule.head.sig()).unwrap() -= 1);
        }
    }
    g
}

