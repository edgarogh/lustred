use crate::gates::Gate;
use either::Either;
use petgraph::prelude::*;
use petgraph::visit::{
    Data, GraphBase, GraphRef, IntoNeighbors, IntoNeighborsDirected, IntoNodeIdentifiers, NodeRef,
    Visitable,
};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter, Write};
use std::iter::Cloned;
use typed_arena::Arena;

/// A Lustre-compatible boolean expression
///
/// Warning: binary expressions should not be built directly from the variant, but rather using the
/// [Expr::and], [Expr::or], [Expr::xor] and similar method. Failing to do so can lead to broken
/// equality comparisons.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Expr<'a> {
    InputTerminal(&'a str),
    Const(bool),

    Not(&'a Expr<'a>),
    And([&'a Expr<'a>; 2]),
    Or([&'a Expr<'a>; 2]),

    Xor([&'a Expr<'a>; 2]),
    Nand([&'a Expr<'a>; 2]),

    // TODO: Implies(),
    PreTerminal(&'a Expr<'a>),
}

impl<'a> PartialOrd for Expr<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Expr<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        // We start by comparing discriminants
        match self.ordinal().cmp(&other.ordinal()) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => {
                // ...then compare on a case-by-case basis

                match (self, other) {
                    (Self::InputTerminal(a), Self::InputTerminal(b)) => a.cmp(b),
                    (Self::Const(a), Self::Const(b)) => a.cmp(b),
                    (a, b) => Iterator::cmp(
                        a.inner_expressions().into_iter(),
                        b.inner_expressions().into_iter(),
                    ),
                }
            }
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InputTerminal(s) => f.write_str(*s),
            Self::Const(true) => f.write_char('⊤'),
            Self::Const(false) => f.write_char('⊥'),
            Self::Not(e) => f.write_fmt(format_args!("!{}", e)),
            Self::And([e1, e2]) => f.write_fmt(format_args!("({} && {})", e1, e2)),
            Self::Or([e1, e2]) => f.write_fmt(format_args!("({} || {})", e1, e2)),
            Self::Xor([e1, e2]) => f.write_fmt(format_args!("({} != {})", e1, e2)),
            Self::Nand([e1, e2]) => f.write_fmt(format_args!("({} !& {})", e1, e2)),
            Self::PreTerminal(e) => f.write_fmt(format_args!("pre {}", e)),
        }
    }
}

impl<'a> Expr<'a> {
    fn ordinal(&self) -> impl Copy + Ord {
        match self {
            Self::InputTerminal(_) => 0,
            Self::Const(_) => 1,
            Self::Not(_) => 2,
            Self::And(_) => 3,
            Self::Or(_) => 4,
            Self::Xor(_) => 5,
            Self::Nand(_) => 6,
            Self::PreTerminal(_) => 7,
        }
    }

    pub fn inner_expressions(&self) -> &[&Self] {
        use std::slice::from_ref;

        match self {
            Self::InputTerminal(_) => &[],
            Self::Const(_) => &[],
            Self::Not(e) => from_ref(e),
            Self::And(es) => es.as_slice(),
            Self::Or(es) => es.as_slice(),
            Self::Xor(es) => es.as_slice(),
            Self::Nand(es) => es.as_slice(),
            Self::PreTerminal(e) => from_ref(e),
        }
    }

    pub fn to_gate(&self) -> Option<&'static Gate> {
        use crate::gates::*;

        match self {
            Self::Not(_) => Some(NOT),
            Self::And(_) => Some(AND),
            Self::Or(_) => Some(OR),
            Self::Xor(_) => Some(XOR),
            Self::Nand(_) => Some(NAND),
            _ => None,
        }
    }

    pub fn and(a: &'a Self, b: &'a Self) -> Self {
        let mut operands = [a, b];
        operands.sort();
        Self::And(operands)
    }

    pub fn or(a: &'a Self, b: &'a Self) -> Self {
        let mut operands = [a, b];
        operands.sort();
        Self::Or(operands)
    }

    pub fn xor(a: &'a Self, b: &'a Self) -> Self {
        let mut operands = [a, b];
        operands.sort();
        Self::Xor(operands)
    }

    pub fn nand(a: &'a Self, b: &'a Self) -> Self {
        let mut operands = [a, b];
        operands.sort();
        Self::Nand(operands)
    }

    pub fn simplify(&'a self, a: &'a Arena<Self>) -> &'a Self {
        match self {
            Self::Not(Self::Const(false)) => a.alloc(Expr::Const(true)),
            Self::Not(Self::Const(true)) => a.alloc(Expr::Const(false)),
            Self::Not(Self::Not(x)) => x.simplify(a),

            // And
            Self::And([Self::Const(false), _]) | Self::And([_, Self::Const(false)]) => {
                a.alloc(Expr::Const(false))
            }
            Self::And([Self::Const(true), x]) | Self::And([x, Self::Const(true)]) => x.simplify(a),
            Self::And([x, Self::Not(y)]) | Self::And([Self::Not(y), x]) if x == y => {
                a.alloc(Expr::Const(false))
            }

            // Or
            Self::Or([Self::Const(true), _]) | Self::Or([_, Self::Const(true)]) => {
                a.alloc(Expr::Const(true))
            }
            Self::Or([Self::Const(false), x]) | Self::Or([x, Self::Const(false)]) => x.simplify(a),
            Self::Or([x, Self::Not(y)]) | Self::Or([Self::Not(y), x]) if x == y => {
                a.alloc(Expr::Const(true))
            }

            // TODO: more aggressive but expensive simplifications by rearranging and/or/xors of
            //  arity > 2 in all possible combinations ?
            other => other,
        }
    }

    /// Apply simplifications that are effective specifically for Redstone circuits
    ///
    /// For instance, a Redstone `NAND(x)` is 3x faster than a `NOT(AND(x))`.
    pub fn simplify_for_redstone(&'a self, a: &'a Arena<Self>) -> &'a Self {
        match self {
            Self::Not(Self::And([x, y])) => a.alloc(Self::Nand([*x, *y])),
            other => other,
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        self == other || self.inner_expressions().iter().any(|e| e.contains(other))
    }
}

#[macro_export]
macro_rules! expr {
    ($a:expr; true) => { Arena::alloc($a, Expr::Const(true)) };
    ($a:expr; false) => { Arena::alloc($a, Expr::Const(false)) };
    ($a:expr; $name:literal) => { Arena::alloc($a, Expr::InputTerminal($name)) };
    ($a:expr; & $value:expr) => { Arena::alloc($a, $value) };
    ($a:expr; $value:ident) => { $value };
    ($a:expr; !  ($($x:tt)*)) => { Arena::alloc($a, Expr::Not(expr!($a; $($x)*))) };
    ($a:expr; ($($x:tt)*) && ($($y:tt)*)) => { Arena::alloc($a, Expr::and(
        expr!($a; $($x)*),
        expr!($a; $($y)*),
    )) };
    ($a:expr; ($($x:tt)*) || ($($y:tt)*)) => { Arena::alloc($a, Expr::or(
        expr!($a; $($x)*),
        expr!($a; $($y)*),
    )) };
    ($a:expr; ($($x:tt)*) != ($($y:tt)*)) => { Arena::alloc($a, Expr::xor(
        expr!($a; $($x)*),
        expr!($a; $($y)*),
    )) };
    ($a:expr; ($($x:tt)*) !& ($($y:tt)*)) => { Arena::alloc($a, Expr::Xor(
        expr!($a; $($x)*),
        expr!($a; $($y)*),
    )) };
}

/// A Lustre-compatible expression that returns multiple boolean values
pub struct MultiExpr<'a>(HashMap<&'a str, &'a Expr<'a>>);

impl<'a> GraphBase for MultiExpr<'a> {
    // type EdgeId = !; // TODO: when stabilized
    type EdgeId = ();
    type NodeId = &'a Expr<'a>;
}

impl<'a> Data for MultiExpr<'a> {
    type EdgeWeight = Self::EdgeId;
    type NodeWeight = Self::NodeId;
}

impl<'a> IntoNeighbors for &MultiExpr<'a> {
    type Neighbors = Cloned<std::slice::Iter<'a, Self::NodeId>>;

    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        a.inner_expressions().into_iter().cloned()
    }
}

impl<'a> IntoNeighborsDirected for &MultiExpr<'a> {
    type NeighborsDirected = Either<Self::Neighbors, <Vec<Self::NodeId> as IntoIterator>::IntoIter>;

    fn neighbors_directed(self, n: Self::NodeId, d: Direction) -> Self::NeighborsDirected {
        match d {
            Direction::Outgoing => Either::Left(self.neighbors(n)),

            // FIXME: that's quite slow I guess
            _ => Either::Right(
                self.node_identifiers()
                    .filter(|n| n.inner_expressions().contains(&n))
                    .collect::<Vec<_>>()
                    .into_iter(),
            ),
        }
    }
}

impl<'a> IntoNodeIdentifiers for &MultiExpr<'a> {
    type NodeIdentifiers = <HashSet<Self::NodeId> as IntoIterator>::IntoIter;

    fn node_identifiers(self) -> Self::NodeIdentifiers {
        let mut nodes = HashSet::new();

        fn add_node<'a>(nodes: &mut HashSet<&'a Expr<'a>>, expr: &'a Expr<'a>) {
            nodes.insert(expr);

            for sub_expr in expr.inner_expressions() {
                add_node(nodes, *sub_expr);
            }
        }

        for expr in self.0.values() {
            add_node(&mut nodes, *expr);
        }

        nodes.into_iter()
    }
}

impl<'a> Visitable for &MultiExpr<'a> {
    type Map = HashSet<Self::NodeId>;

    fn visit_map(self: &Self) -> Self::Map {
        Self::Map::default()
    }

    fn reset_map(self: &Self, map: &mut Self::Map) {
        map.clear();
    }
}

impl<'a> MultiExpr<'a> {
    pub fn new(outputs: HashMap<&'a str, &'a Expr<'a>>) -> Self {
        Self(outputs)
    }

    pub fn dfs(&self) -> impl Iterator<Item = Self::NodeId> {
        self.0.values().flat_map(|e| Dfs::new(self, *e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check if commutativity is correctly handled when checking for equality
    #[test]
    fn test_eq() {
        let arena = Arena::new();

        assert_eq!(
            expr!(&arena; (true) && (false)),
            expr!(&arena; (false) && (true)),
        );
    }

    #[test]
    fn test_simplify_not() {
        let arena = Arena::new();
        let e = expr!(&arena; !(true));
        assert!(matches!(e.simplify(&arena), Expr::Const(false)));
        let e = expr!(&arena; !(false));
        assert!(matches!(e.simplify(&arena), Expr::Const(true)));
    }

    #[test]
    fn test_simplify_and() {
        let arena = Arena::new();
        let e = expr!(&arena; ("c") && (false));
        assert!(matches!(e.simplify(&arena), Expr::Const(false)));
        let e = expr!(&arena; (true) && (true));
        assert!(matches!(e.simplify(&arena), Expr::Const(true)));
    }

    #[test]
    fn test_simplify_or() {
        let arena = Arena::new();
        let e = expr!(&arena; ("c") || (true));
        assert!(matches!(e.simplify(&arena), Expr::Const(true)));
        let e = expr!(&arena; (false) || (false));
        assert!(matches!(e.simplify(&arena), Expr::Const(false)));
    }
}
