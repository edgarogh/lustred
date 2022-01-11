use crate::expression::{Expr, MultiExpr};
use crate::gates::Gate;
use petgraph::prelude::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::RangeInclusive;
use typed_arena::Arena;

mod expression;
mod gates;

#[derive(Clone, Debug)]
enum LayerContents {
    Gate(usize, &'static Gate),
    Bridge(RangeInclusive<usize>),
}

impl LayerContents {
    pub fn range(&self) -> RangeInclusive<usize> {
        match self {
            Self::Gate(start, gate) => *start..=(*start + gate.width - 1),
            Self::Bridge(range) => range.clone(),
        }
    }
}

struct Layer<'a> {
    expr: &'a Expr<'a>,
    contents: LayerContents,
}

impl<'a> Debug for Layer<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let range = self.contents.range();

        f.write_fmt(format_args!(
            "{expr:64} | {v:indent$}{v:█<width$}",
            expr = self.expr.to_string(),
            indent = *range.start(),
            width = range.clone().count(),
            v = "",
        ))
    }
}

fn adder() {
    let nodes = Arena::new();

    let a = nodes.alloc(Expr::InputTerminal("a"));
    let b = nodes.alloc(Expr::InputTerminal("b"));
    let c = nodes.alloc(Expr::InputTerminal("c"));

    let xor1 = expr!(&nodes; (a) != (b));
    let nand1 = expr!(&nodes; !((c) && (xor1)));
    let nand2 = expr!(&nodes; !((a) && (b)));

    let s = nodes.alloc(Expr::xor(xor1, c));
    let c = nodes.alloc(Expr::Not(nodes.alloc(Expr::and(nand1, nand2))));

    println!("{}\n{}", s, c);

    let mut g = DiGraph::new();
    let mut cache = HashMap::new();

    fn add_node<'a>(
        g: &mut Graph<String, &'static str>,
        c: &mut HashMap<&'a Expr<'a>, NodeIndex>,
        expr: &'a Expr<'a>,
    ) -> NodeIndex {
        match c.entry(expr) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let node_label = match expr.to_gate() {
                    None => format!("{}", expr),
                    Some(gate) => format!(
                        "{} (delay={}, length={})\n{}",
                        gate.name, gate.delay, gate.length, expr,
                    ),
                };

                let idx = g.add_node(node_label);
                e.insert(idx);

                for sub_expr in expr.inner_expressions() {
                    let sub_idx = add_node(g, c, *sub_expr);
                    g.add_edge(idx, sub_idx, Default::default());
                }

                idx
            }
        }
    }

    let o1 = add_node(&mut g, &mut cache, s);
    let o2 = add_node(&mut g, &mut cache, c);

    let inputs = cache
        .iter()
        .filter_map(|(e, idx)| match e {
            Expr::InputTerminal(_) => Some(idx),
            _ => None,
        })
        .map(|x| x.index().to_string())
        .collect::<Vec<_>>();

    let outputs = [(o1, "s"), (o2, "c")]
        .into_iter()
        .map(|(output_expression, name)| {
            let output_node = g.add_node(name.into());
            g.add_edge(output_node, output_expression, Default::default());
            output_node
        })
        .map(|x| x.index().to_string())
        .collect::<Vec<_>>();

    let header = format!(
        "digraph {{\n  rankdir=BT;subgraph clusterO {{ label=outputs; {} }} subgraph cluster1 {{ label=inputs; {} }}",
        outputs.join(","),
        inputs.join(",")
    );

    dbg!(cache.len());

    let mut dot = header;
    dot += &petgraph::dot::Dot::with_attr_getters(&g, &[], &|_, _| String::new(), &|g, (i, _)| {
        let expr = cache
            .iter()
            .filter(|(_, v)| **v == i)
            .map(|(k, _)| *k)
            .next();

        if let Some(gate) = expr.and_then(|expr| expr.to_gate()) {
            format!("shape=rect height={}", gate.length as f32 / 3.)
        } else {
            Default::default()
        }
    })
    .to_string()["digraph {".len()..];

    std::fs::write("adder.dot", dot).unwrap();

    let mut layers = Vec::with_capacity(nodes.len());

    fn collect_expr<'a>(layers: &mut Vec<&'a Expr<'a>>, expr: &'a Expr<'a>) {
        layers.push(expr);
        for sub_expr in expr.inner_expressions() {
            collect_expr(layers, *sub_expr);
        }
    }

    for output in [s as &_, c] {
        collect_expr(&mut layers, output);
    }

    let mut outputs = HashMap::<_, &_>::with_capacity(2);
    outputs.insert("s", s);
    outputs.insert("c", c);
    let mxp = MultiExpr::new(outputs);

    let sorted = petgraph::algo::toposort(&mxp, None).unwrap();
    let mut layers = Vec::with_capacity(2 * sorted.len());

    for node in mxp.dfs() {
        if let Some(gate) = expr.to_gate() {
            layers.push(Layer {
                expr,
                contents: LayerContents::Gate(3, gate),
            })
        } else {
            // TODO: bridge to input
            // layers.push(Layer { expr, contents:  })
        }
    }

    dbg!(&layers);
}

fn main() {
    adder();
}
