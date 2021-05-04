lalrpop_mod!(parser);
use parser::ProgramParser;
use std::fmt;
use std::collections::{HashSet, HashMap};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Term {
    Number(i32),
    Variable(String),
    Function(String, Vec<Term>),
}

pub type Substitution = HashMap<String, Term>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Atom {
    pub name: String,
    pub args: Vec<Term>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Relation {
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    Inequal,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Literal {
    Literal{positive: bool, atom: Atom},
    Comparison{lhs: Term, rel: Relation, rhs: Term},
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum AggregateFunction {
    Count,
    SumP,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct AggregateElement {
    pub terms: Vec<Term>,
    pub condition: Vec<Atom>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Aggregate {
    pub fun: AggregateFunction,
    pub elements: Vec<AggregateElement>,
    pub rel: Relation,
    pub guard: Term,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum BodyLiteral {
    Literal(Literal),
    Aggregate(Aggregate),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<BodyLiteral>,
}

impl Atom {
    pub fn sig(&self) -> (&str, usize) {
        return (&self.name, self.args.len());
    }
}

// Note: not sure how to do this right.
macro_rules! write_sep {
    ($f:expr, $seq:expr) => (
        write_sep!($f, $seq, ",")
    );
    ($f:expr, $seq:expr, $sep:expr) => (
        || -> fmt::Result {
            let mut comma = false;
            for term in $seq {
                if comma {
                    write!($f, $sep)?;
                }
                else {
                    comma = true;
                }
                write!($f, "{}", term)?;
            }
            Ok(())
        }()
    )
}

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Relation::LessThan => write!(f, "<"),
            Relation::LessEqual => write!(f, "<="),
            Relation::GreaterEqual => write!(f, ">="),
            Relation::GreaterThan => write!(f, ">"),
            Relation::Equal => write!(f, "="),
            Relation::Inequal => write!(f, "!="),
        }
    }
}

impl fmt::Display for AggregateFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AggregateFunction::Count => write!(f, "#count"),
            AggregateFunction::SumP => write!(f, "#sum+"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Number(x) => write!(f, "{}", x),
            Term::Variable(x) => write!(f, "{}", x),
            Term::Function(name, args) => {
                write!(f, "{}", name)?;
                if args.len() > 0 {
                    write!(f, "(")?;
                    write_sep!(f, args)?;
                    write!(f, ")")?;
                }
                return Ok(());
            }
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.args.len() > 0 {
            write!(f, "(")?;
            write_sep!(f, &self.args)?;
            write!(f, ")")?;
        }
        return Ok(());
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Comparison{lhs, rel, rhs} => {
                write!(f, "{}{}{}", lhs, rel, rhs)
            },
            Literal::Literal{positive, atom} => {
                if !positive {
                    write!(f, "not ")?;
                }
                write!(f, "{}", atom)?;
                return Ok(());
            }
        }
    }
}

impl fmt::Display for AggregateElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_sep!(f, &self.terms)?;
        write!(f, ":")?;
        write_sep!(f, &self.condition)?;
        return Ok(());
    }
}

impl fmt::Display for Aggregate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fun)?;
        write!(f, " {{ ")?;
        write_sep!(f, &self.elements, "; ")?;
        write!(f, " }} {} {}", self.rel, self.guard)?;
        return Ok(());
    }
}

impl fmt::Display for BodyLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BodyLiteral::Literal(lit) => write!(f, "{}", lit),
            BodyLiteral::Aggregate(aggr) => write!(f, "{}", aggr),
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.head)?;
        if !self.body.is_empty() {
            write!(f, " :- ")?;
            write_sep!(f, &self.body, ", ")?;
        }
        return Ok(());
    }
}

pub trait ClassifyPosNeg {
    fn has_positive(&self) -> bool;
    fn has_negative(&self) -> bool;
}

impl ClassifyPosNeg for Aggregate {
    fn has_positive(&self) -> bool {
        match (&self.fun, &self.rel) {
            (_, Relation::Equal) => true,
            (_, Relation::Inequal) => true,
            (AggregateFunction::Count, Relation::GreaterThan) => true,
            (AggregateFunction::SumP, Relation::GreaterThan) => true,
            (AggregateFunction::Count, Relation::GreaterEqual) => true,
            (AggregateFunction::SumP, Relation::GreaterEqual) => true,
            _ => false,
        }
    }
    fn has_negative(&self) -> bool {
        match (&self.fun, &self.rel) {
            (_, Relation::Equal) => true,
            (_, Relation::Inequal) => true,
            (AggregateFunction::Count, Relation::LessThan) => true,
            (AggregateFunction::SumP, Relation::LessThan) => true,
            (AggregateFunction::Count, Relation::LessEqual) => true,
            (AggregateFunction::SumP, Relation::LessEqual) => true,
            _ => false,
        }
    }
}

impl ClassifyPosNeg for Literal {
    fn has_positive(&self) -> bool {
        match &self {
            Literal::Literal{positive, ..} => *positive,
            _ => false,
        }
    }
    fn has_negative(&self) -> bool {
        match &self {
            Literal::Literal{positive, ..} => !*positive,
            _ => false,
        }
    }
}

impl ClassifyPosNeg for BodyLiteral {
    fn has_positive(&self) -> bool {
        match &self {
            BodyLiteral::Literal(lit) => lit.has_positive(),
            BodyLiteral::Aggregate(aggr) => aggr.has_positive(),
        }
    }
    fn has_negative(&self) -> bool {
        match &self {
            BodyLiteral::Literal(lit) => lit.has_negative(),
            BodyLiteral::Aggregate(aggr) => aggr.has_negative(),
        }
    }
}

pub trait HasVariables {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool);

    fn get_variables(&self, bound: bool) -> HashSet<String> {
        let mut ret = HashSet::new();
        self.add_variables(&mut ret, bound);
        ret
    }

    fn apply(&self, s: &Substitution) -> Self;
}

impl<T> HasVariables for Vec<T> where T: HasVariables {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        for x in self {
            x.add_variables(variables, bound);
        }
    }

    fn apply(&self, s: &Substitution) -> Self {
        self.iter().map(|x| x.apply(s)).collect()
    }

}

impl HasVariables for Term {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        match self {
            Term::Number(..) => (),
            Term::Variable(x) => { variables.insert(x.to_string()); () },
            Term::Function(_, args) => args.add_variables(variables, bound),
        }
    }

    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Term::Variable(x) => {
                if let Some(s) = s.get(x) {
                    s.clone()
                }
                else {
                    self.clone()
                }
            }
            Term::Number(..) => {
                self.clone()
            }
            Term::Function(name, args) => {
                Term::Function(name.clone(), args.iter().map(|arg| { arg.apply(s) }).collect())
            }
        }
    }
}

impl HasVariables for Atom {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        self.args.add_variables(variables, bound);
    }

    fn apply(&self, s: &Substitution) -> Atom {
        Atom{name: self.name.clone(), args: self.args.iter().map(|arg| { arg.apply(s) }).collect()}
    }
}

impl HasVariables for Literal {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        match self {
            Literal::Literal{atom, positive} if *positive || !bound => { 
                atom.add_variables(variables, bound);
            },
            Literal::Comparison{lhs, rhs, ..} if !bound => {
                lhs.add_variables(variables, bound);
                rhs.add_variables(variables, bound);
            },
            _ => (),
        }
    }

    fn apply(&self, s: &Substitution) -> Literal {
        match self {
            Literal::Literal{positive, atom} => {
                Literal::Literal{atom: atom.apply(s), positive: *positive}
            }
            Literal::Comparison{lhs, rel, rhs} => {
                Literal::Comparison{lhs: lhs.apply(s), rhs: rhs.apply(s), rel: rel.clone()}
            }
        }
    }
}

impl HasVariables for AggregateElement {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        if !bound {
            self.terms.add_variables(variables, bound);
        }
        self.condition.add_variables(variables, bound);
    }

    fn apply(&self, s: &Substitution) -> Self {
        AggregateElement{terms: self.terms.apply(s), condition: self.condition.apply(s)}
    }
}

impl HasVariables for Aggregate {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        if !bound {
            self.guard.add_variables(variables, bound);
        }
    }
    fn apply(&self, s: &Substitution) -> Self {
        Aggregate{elements: self.elements.apply(s), guard: self.guard.apply(s), ..*self}
    }
}

impl HasVariables for BodyLiteral {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        match self {
            BodyLiteral::Literal(lit) => lit.add_variables(variables, bound),
            BodyLiteral::Aggregate(aggr) => aggr.add_variables(variables, bound),
        }
    }
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            BodyLiteral::Literal(lit) => BodyLiteral::Literal(lit.apply(s)),
            BodyLiteral::Aggregate(aggr) => BodyLiteral::Aggregate(aggr.apply(s)),
        }
    }
}

impl HasVariables for Rule {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        if !bound {
            self.head.add_variables(variables, bound);
        }
        self.body.add_variables(variables, bound);
    }
    fn apply(&self, s: &Substitution) -> Self {
        Rule{head: self.head.apply(s), body: self.body.apply(s)}
    }
}

pub fn parse(prg: &str) -> Result<Vec<Rule>, String> {
    match ProgramParser::new().parse(prg) {
        Ok(prg) => Ok(prg),
        Err(e) => Err(format!("{}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert!(ProgramParser::new().parse("((22)").is_err());
        assert!(ProgramParser::new().parse("a.").is_ok());
        assert!(ProgramParser::new().parse("a(X).").is_ok());
        assert!(ProgramParser::new().parse("p(X) :- q(f(X),1), not r(Y), X<Y, #count { 1,X: p(X) } > 0.").is_ok());
    }
}
