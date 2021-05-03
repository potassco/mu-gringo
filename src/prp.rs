use crate::ast::*;
use std::collections::BTreeSet;

pub type Domain = BTreeSet<Atom>;

pub struct PropagateState<'a> {
    alpha: &'a Vec<(Aggregate, Vec<String>)>,
}

impl<'a> PropagateState<'a> {
    pub fn new(alpha: &'a Vec<(Aggregate, Vec<String>)>) -> Self {
        PropagateState{alpha}
    }

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
