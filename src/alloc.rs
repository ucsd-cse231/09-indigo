use std::cmp::max;

use im::{hashmap, hashset, HashMap, HashSet};

use crate::expr::{free_vars_defn, Defn, Expr, Loc, Reg};
use heuristic_graph_coloring::*;

pub fn reg_alloc(defn: &Defn, free_xs: &HashSet<String>, regs: &[Reg], offset: usize) -> Alloc {
    let res = alloc_locals(defn, free_xs, regs, offset).concat(alloc_params(defn));
    // println!("TRACE: reg_alloc with {regs:?} = {res:?}");
    res
}

#[derive(Debug)]
pub struct Alloc(HashMap<String, Loc>, usize);

impl Alloc {
    pub fn lookup_var(&self, x: &str) -> Loc {
        match self.0.get(x) {
            None => panic!("Unbound variable {}", x),
            Some(loc) => *loc,
        }
    }

    pub fn used_regs(&self) -> Vec<Reg> {
        let regs: HashSet<Reg> = self
            .0
            .values()
            .filter_map(|l| if let Loc::Reg(r) = l { Some(*r) } else { None })
            .collect();
        let mut regs: Vec<Reg> = regs.into_iter().collect();
        regs.sort();
        regs
    }

    pub fn stack_size(&self) -> usize {
        self.0
            .values()
            .filter_map(|l| match l {
                Loc::Stack(i) if *i >= 0 => Some(*i),
                _ => None,
            })
            .max()
            .unwrap_or(0) as usize
    }

    fn concat(self, other: Alloc) -> Alloc {
        let n = max(self.1, other.1);
        Alloc(self.0.into_iter().chain(other.0).collect(), n)
    }

    pub fn num_regs(&self) -> usize {
        self.1
    }
}

// --------------------------------------------------------------------------------------------------
// Compute the Alloc for local variables of a function definition
// --------------------------------------------------------------------------------------------------

fn alloc_locals(defn: &Defn, free_xs: &HashSet<String>, regs: &[Reg], offset: usize) -> Alloc {
    let mut graph = ConflictGraph::new();
    // 1. add conflicts for free vars
    for x in free_xs {
        graph.add_variable(x);
        for y in free_xs {
            if x != y {
                graph.add_conflict(x, y);
            }
        }
    }
    // 2. add conflicts for body
    let params = HashSet::from(&defn.params);
    live(&mut graph, &defn.body, &hashset! {}, &params, &hashset! {});

    // 3. allocate local binders and free vars
    Alloc(graph.allocate(regs, offset), regs.len())
}

// --------------------------------------------------------------------------------------------------
// Compute the Alloc for parameters of a function definition
// --------------------------------------------------------------------------------------------------

fn alloc_params(defn: &Defn) -> Alloc {
    let mut alloc = hashmap! {};

    let defn_name = if let Some(name) = &defn.name {
        name
    } else {
        ""
    };
    let mut params = vec![defn_name];

    for param in &defn.params {
        params.push(param)
    }
    for (i, param) in params.into_iter().enumerate() {
        let pos = -2 - (i as i32);
        alloc.insert(param.to_string(), Loc::Stack(pos));
    }
    Alloc(alloc, 0)
}

// --------------------------------------------------------------------------------------------------
// Find the live-variables and conflicts of an `Expr`
// --------------------------------------------------------------------------------------------------

#[allow(unused_variables)]
fn live(
    graph: &mut ConflictGraph,
    e: &Expr,
    binds: &HashSet<String>,
    params: &HashSet<String>,
    out: &HashSet<String>,
) -> HashSet<String> {
    match e {
        Expr::Num(_) | Expr::Input | Expr::True | Expr::False => out.clone(),
        Expr::Let(x, e1, e2) => {
            todo!("fill this in")
        }
        Expr::Add1(e)
        | Expr::Sub1(e)
        | Expr::Neg(e)
        | Expr::Break(e)
        | Expr::Print(e)
        | Expr::Get(e, _) => live(graph, e, binds, params, out),
        Expr::Loop(e) => {
            todo!("fill this in")
        }
        Expr::Var(x) => live_var(x, params, out),
        Expr::Plus(e1, e2)
        | Expr::Mult(e1, e2)
        | Expr::Eq(e1, e2)
        | Expr::Le(e1, e2)
        | Expr::Vek(e1, e2) => {
            todo!("fill this in")
        }
        Expr::If(e1, e2, e3) => {
            todo!("fill this in")
        }
        Expr::Set(x, e1) => {
            todo!("fill this in")
        }
        Expr::Block(es) => {
            todo!("fill this in")
        }
        Expr::Call(f, es) => {
            todo!("fill this in")
        }
        Expr::Fun(defn) => out
            .clone()
            .union(free_vars_defn(defn).difference(params.clone())),
    }
}

fn live_var(x: &String, params: &HashSet<String>, out: &HashSet<String>) -> HashSet<String> {
    if params.contains(x) {
        out.clone()
    } else {
        out.clone().union(hashset! {x.clone()})
    }
}

// --------------------------------------------------------------------------------------------------
// Conflict Graph Structure: used to record conflicts between variables and then compute coloring
// --------------------------------------------------------------------------------------------------

struct ConflictGraph {
    /// Number of vertices in the graph
    vars: usize,
    /// Edges in the graph
    edges: HashSet<(usize, usize)>,
    /// Map from identifiers to vertex index
    idents: HashMap<String, usize>,
}

impl ConflictGraph {
    fn new() -> Self {
        Self {
            vars: 0,
            edges: HashSet::new(),
            idents: HashMap::new(),
        }
    }

    fn add_variable(&mut self, x: &str) -> usize {
        let x = *self.idents.entry(x.to_string()).or_insert_with(|| {
            let idx = self.vars;
            self.vars += 1;
            idx
        });
        x
    }

    fn add_conflict(&mut self, x: &str, y: &str) {
        let x = self.add_variable(x);
        let y = self.add_variable(y);
        self.edges.insert((x, y));
    }

    fn color(&self) -> HashMap<String, usize> {
        let mut graph = VecVecGraph::new(self.vars);
        for (x, y) in &self.edges {
            graph.add_edge(*x, *y);
        }
        let node_colors = color_greedy_by_degree(&graph);
        let mut colors = HashMap::new();
        for (x, i) in &self.idents {
            colors.insert(x.clone(), node_colors[*i]);
        }
        colors
    }

    #[allow(unused_mut)]
    #[allow(unused_variables)]
    fn allocate(&self, regs: &[Reg], offset: usize) -> HashMap<String, Loc> {
        let n = regs.len();
        let colors = self.color();
        let mut alloc = HashMap::new();
        todo!("use `colors` to fill in the values for `alloc`");
        alloc
    }
}

// --------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut cg = ConflictGraph::new();
        cg.add_conflict("x", "y");
        cg.add_conflict("y", "z");
        cg.add_conflict("x", "z");
        cg.add_conflict("z", "w");
        // let coloring = cg.color();
        // println!("{:?}", coloring);
        let coloring = cg.allocate(&[Reg::R8, Reg::R9], 0);
        println!("{:?}", coloring)
    }
}
