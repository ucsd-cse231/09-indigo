use crate::expr::{ite, set, Defn, Expr::*};
use crate::expr::{lets, Expr};

pub fn anf(e: &Expr) -> Expr {
    AnfContext::new().anf(e)
}

struct AnfContext {
    idx: usize,
}

impl AnfContext {
    fn new() -> Self {
        AnfContext { idx: 0 }
    }

    fn fresh(&mut self) -> String {
        let idx = self.idx;
        self.idx += 1;
        format!("anf{idx}")
    }

    fn anf1(&mut self, e: &Expr, k: impl Fn(Box<Expr>) -> Expr) -> Expr {
        let (bs, i) = self.imm(e);
        lets(bs, k(Box::new(i)))
    }

    fn anf2(&mut self, e1: &Expr, e2: &Expr, k: impl Fn(Box<Expr>, Box<Expr>) -> Expr) -> Expr {
        let (mut bs, i1) = self.imm(e1);
        let (bs2, i2) = self.imm(e2);
        bs.extend(bs2);
        lets(bs, k(Box::new(i1), Box::new(i2)))
    }

    fn anf(&mut self, e: &Expr) -> Expr {
        match e {
            Input | Var(_) | Num(_) | Expr::True | Expr::False => e.clone(),
            Add1(e) => self.anf1(e, Add1),
            Sub1(e) => self.anf1(e, Sub1),
            Neg(e) => self.anf1(e, Neg),
            Print(e) => self.anf1(e, Print),
            Plus(e1, e2) => self.anf2(e1, e2, |i1, i2| Plus(i1, i2)),
            Mult(e1, e2) => self.anf2(e1, e2, |i1, i2| Mult(i1, i2)),
            Eq(e1, e2) => self.anf2(e1, e2, |i1, i2| Eq(i1, i2)),
            Le(e1, e2) => self.anf2(e1, e2, |i1, i2| Le(i1, i2)),
            Vek(e1, e2) => self.anf2(e1, e2, |i1, i2| Vek(i1, i2)),
            If(e1, e2, e3) => ite(self.anf(e1), self.anf(e2), self.anf(e3)),
            Let(x, e1, e2) => lets(vec![(x.clone(), self.anf(e1))], self.anf(e2)),
            Set(x, e) => set(x, self.anf(e)),
            Block(es) => Block(es.iter().map(|e| self.anf(e)).collect()),
            Loop(e) => Loop(Box::new(self.anf(e))),
            Break(e) => Break(Box::new(self.anf(e))),
            Get(e, idx) => Get(Box::new(self.anf(e)), *idx),
            Call(f, es) => {
                let mut binds = vec![];
                let mut is = vec![];
                for e in es {
                    let (bs, i) = self.imm(e);
                    binds.extend(bs);
                    is.push(i);
                }
                lets(binds, Call(f.clone(), is))
            }
            Fun(defn) => Fun(self.anf_defn(defn)),
        }
    }

    fn anf_defn(&mut self, defn: &Defn) -> Defn {
        let name = defn.name.clone();
        let params = defn.params.clone();
        let body = Box::new(self.anf(&defn.body));
        Defn { name, params, body }
    }

    fn imm_expr(&mut self, e: &Expr) -> (Vec<(String, Expr)>, Expr) {
        let a = self.anf(e);
        let x = self.fresh();
        let i = Var(x.clone());
        (vec![(x, a)], i)
    }

    fn imm1(&mut self, e: &Expr, k: impl Fn(Box<Expr>) -> Expr) -> (Vec<(String, Expr)>, Expr) {
        let (mut bs, i) = self.imm(e);
        let x = self.fresh();
        bs.push((x.clone(), k(Box::new(i))));
        (bs, Var(x))
    }

    fn imm2(
        &mut self,
        e1: &Expr,
        e2: &Expr,
        k: impl Fn(Box<Expr>, Box<Expr>) -> Expr,
    ) -> (Vec<(String, Expr)>, Expr) {
        let (mut bs, i1) = self.imm(e1);
        let (bs2, i2) = self.imm(e2);
        bs.extend(bs2);
        let x = self.fresh();
        bs.push((x.clone(), k(Box::new(i1), Box::new(i2))));
        (bs, Var(x))
    }

    fn imm(&mut self, e: &Expr) -> (Vec<(String, Expr)>, Expr) {
        match e {
            Num(_) | Var(_) | Input | True | False => (vec![], e.clone()),
            If(_, _, _)
            | Let(_, _, _)
            | Block(_)
            | Loop(_)
            | Break(_)
            | Print(_)
            | Get(_, _)
            | Fun(_) => self.imm_expr(e),
            Add1(e) => self.imm1(e, Add1),
            Sub1(e) => self.imm1(e, Sub1),
            Neg(e) => self.imm1(e, Neg),
            Set(x, e) => self.imm1(e, |i| Set(x.clone(), i)),
            Plus(e1, e2) => self.imm2(e1, e2, Plus),
            Mult(e1, e2) => self.imm2(e1, e2, Mult),
            Eq(e1, e2) => self.imm2(e1, e2, Eq),
            Le(e1, e2) => self.imm2(e1, e2, Le),
            Vek(e1, e2) => self.imm2(e1, e2, Vek),
            Call(f, es) => {
                let mut binds = vec![];
                let mut is = vec![];
                for e in es {
                    let (bs, i) = self.imm(e);
                    binds.extend(bs);
                    is.push(i);
                }
                let x = self.fresh();
                binds.push((x.clone(), Call(f.clone(), is)));
                (binds, Var(x))
            }
        }
    }
}
