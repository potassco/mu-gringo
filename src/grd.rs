use crate::ast::*;
use std::collections::{BTreeSet, HashMap};

type Domain = BTreeSet<Atom>;

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
fn matches(s: &Substitution, dom_i: &Domain, dom_j: &Domain, dom_jp: &Domain, lit: &Literal) -> Vec<(Substitution, bool)> {
    let mut ret: Vec<(Substitution, bool)> = Vec::new();
    match lit {
        Literal::Literal{positive: true, atom} => {
            let a = atom.apply(s);
            for g in dom_j {
                let mut sp = Substitution{mapping: HashMap::new()};
                if a.ground_match(&g, &mut sp) {
                    sp.mapping.extend(s.mapping.iter().map(|(u, v)| (u.clone(), v.clone())));
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

fn ground_component(dom_i: &Domain, dom_j: &mut Domain, comp: &Vec<&Rule>) -> Vec<Rule> {
    let mut ret = Vec::new();
    let mut dom_jp = Domain::new();
    loop {
        let m = ret.len();
        for rule in comp {
            println!("%       {}", rule);
            ret.append(&mut ground_rule(&Substitution{mapping: HashMap::new()}, dom_i, dom_j, &dom_jp, rule, 0, false));
        }
        dom_jp = dom_j.clone();
        for rule in &ret[m..] {
            println!("%         {}", rule);
            dom_j.insert(rule.head.clone());
        }
        if dom_jp.len() == dom_j.len() {
            break;
        }
    }
    ret
}

fn keep_rule(rule: &Rule, open: &HashMap<(&str, usize), i32>) -> bool {
    rule.body.iter().all(|blit| match blit {
        BodyLiteral::Literal(Literal::Literal{positive: false, atom}) =>
            *open.get(&atom.sig()).unwrap_or(&0) == 0,
        BodyLiteral::Aggregate(..) => 
            panic!("implement me!!!"),
        _ =>
            true,
    })
}

pub fn ground<'a>(seq: &'a Vec<Vec<Vec<&'a Rule>>>) -> Vec<Rule> {
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
            let ref_compp = ref_comp.iter().filter(|rule| keep_rule(rule, &open)).cloned().collect();

            println!("%   Ground Refined Component");
            println!("%     Ground Facts");
            f.extend(ground_component(&dom_j, &mut dom_i, &ref_compp));
            println!("%     Ground Program");
            g.extend(ground_component(&dom_i, &mut dom_j, ref_comp));

            ref_comp.iter().for_each(|rule| *open.get_mut(&rule.head.sig()).unwrap() -= 1);
        }
    }
    g
}

