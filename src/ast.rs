lalrpop_mod!(parser);
use parser::ProgramParser;
use std::fmt;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash)]
pub enum Term {
    Number(i32),
    Variable(String),
    Function(String, Vec<Term>),
}

#[derive(PartialEq, Eq, Hash)]
pub struct Atom {
    pub name: String,
    pub args: Vec<Term>,
}

#[derive(PartialEq, Eq, Hash)]
pub enum Relation {
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

#[derive(PartialEq, Eq, Hash)]
pub enum Literal {
    Literal{positive: bool, atom: Atom},
    Comparison{lhs: Term, rel: Relation, rhs: Term},
}

#[derive(PartialEq, Eq, Hash)]
pub enum AggregateFunction {
    Count,
}

#[derive(PartialEq, Eq, Hash)]
pub struct AggregateElement {
    pub terms: Vec<Term>,
    pub condition: Vec<Atom>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct Aggregate {
    pub fun: AggregateFunction,
    pub elements: Vec<AggregateElement>,
    pub rel: Relation,
    pub guard: Term,
}

#[derive(PartialEq, Eq, Hash)]
pub enum BodyLiteral {
    Literal(Literal),
    Aggregate(Aggregate),
}

#[derive(PartialEq, Eq, Hash)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<BodyLiteral>,
}

macro_rules! write_sep {
    ($f:expr, $seq:expr) => (
        write_sep!($f, $seq, ",")
    );
    ($f:expr, $seq:expr, $sep:expr) => (
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
    )
}

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Relation::LessThan => write!(f, "<"),
            Relation::LessEqual => write!(f, "<="),
            Relation::GreaterEqual => write!(f, ">="),
            Relation::GreaterThan => write!(f, ">"),
        }
    }
}

impl fmt::Display for AggregateFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AggregateFunction::Count => write!(f, "#count"),
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
                    write_sep!(f, args);
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
            write_sep!(f, &self.args);
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
        write_sep!(f, &self.terms);
        write!(f, ":")?;
        write_sep!(f, &self.condition);
        return Ok(());
    }
}

impl fmt::Display for Aggregate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fun)?;
        write!(f, " {{ ")?;
        write_sep!(f, &self.elements, "; ");
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
            write_sep!(f, &self.body, ", ");
        }
        return Ok(());
    }
}

pub trait HasVariables {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool);

    fn get_variables(&self, bound: bool) -> HashSet<String> {
        let mut ret = HashSet::new();
        self.add_variables(&mut ret, bound);
        ret
    }
}

impl<T> HasVariables for Vec<T> where T: HasVariables {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        for x in self {
            x.add_variables(variables, bound);
        }
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
}

impl HasVariables for Atom {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        self.args.add_variables(variables, bound);
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
}

impl HasVariables for AggregateElement {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        if !bound {
            self.terms.add_variables(variables, bound);
        }
        self.condition.add_variables(variables, bound);
    }
}

impl HasVariables for Aggregate {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        if !bound {
            self.guard.add_variables(variables, bound);
        }
    }
}

impl HasVariables for BodyLiteral {
    fn add_variables(&self, variables: &mut HashSet<String>, bound: bool) {
        match self {
            BodyLiteral::Literal(lit) => lit.add_variables(variables, bound),
            BodyLiteral::Aggregate(aggr) => aggr.add_variables(variables, bound),
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
