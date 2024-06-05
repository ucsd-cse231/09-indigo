use im::HashSet;
use sexp::Atom::*;
use sexp::*;

#[derive(Debug, Clone)]
pub struct Defn {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(i32),
    Add1(Box<Expr>),
    Sub1(Box<Expr>),
    Neg(Box<Expr>),
    Var(String),
    Let(String, Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Input,
    True,
    False,
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
    Vek(Box<Expr>, Box<Expr>),
    Get(Box<Expr>, Index),
    Call(String, Vec<Expr>),
    Fun(Defn),
}

#[derive(Debug, Copy, Clone)]
pub enum Index {
    Zero,
    One,
}

impl Index {
    pub fn val(&self) -> usize {
        match self {
            Index::Zero => 0,
            Index::One => 1,
        }
    }
}

#[derive(Debug, Copy, Eq, PartialOrd, Ord, PartialEq, Hash, Clone)]
pub enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RDI,
    RSI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::RDI => write!(f, "rdi"),
            Reg::RSI => write!(f, "rsi"),
            Reg::RAX => write!(f, "rax"),
            Reg::RBX => write!(f, "rbx"),
            Reg::RCX => write!(f, "rcx"),
            Reg::RDX => write!(f, "rdx"),
            Reg::R8 => write!(f, "r8"),
            Reg::R9 => write!(f, "r9"),
            Reg::R10 => write!(f, "r10"),
            Reg::R11 => write!(f, "r11"),
            Reg::R12 => write!(f, "r12"),
            Reg::R13 => write!(f, "r13"),
            Reg::R14 => write!(f, "r14"),
            Reg::R15 => write!(f, "r15"),
        }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Loc::Reg(r) => write!(f, "{}", r),
            Loc::Stack(i) => write!(f, "[rbp - 8*{}]", i),
        }
    }
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Var(loc, _) => write!(f, "{}", loc),
            Arg::Lbl(l) => write!(f, "QWORD [rel {l}]"),
            Arg::Con(n) => write!(f, "{}", n),
        }
    }
}

pub const REGISTERS: [Reg; 8] = [
    Reg::RBX,
    Reg::RDX,
    Reg::R8,
    Reg::R9,
    Reg::R10,
    Reg::R12,
    Reg::R13,
    Reg::R14,
];

#[derive(Debug, Copy, Eq, PartialEq, Clone)]
pub enum Loc {
    /// Register
    Reg(Reg),
    /// Stack local:  [rbp - 8 * offset]
    Stack(i32),
}

#[derive(Debug, Clone)]
pub enum Arg {
    Var(Loc, Option<String>), // variable on stack frame at rbp - 8*i
    Lbl(String),              // code label
    Con(i32),                 // constant
}

pub fn num(n: i32) -> Expr {
    Expr::Num(n)
}

pub fn add1(e: Expr) -> Expr {
    Expr::Add1(Box::new(e))
}

pub fn sub1(e: Expr) -> Expr {
    Expr::Sub1(Box::new(e))
}

pub fn negate(e: Expr) -> Expr {
    Expr::Neg(Box::new(e))
}

pub fn expr0() -> Expr {
    add1(sub1(num(5)))
}

pub fn expr1() -> Expr {
    negate(add1(num(5)))
}

pub fn plus(e1: Expr, e2: Expr) -> Expr {
    Expr::Plus(Box::new(e1), Box::new(e2))
}

pub fn mult(e1: Expr, e2: Expr) -> Expr {
    Expr::Mult(Box::new(e1), Box::new(e2))
}

pub fn eq(e1: Expr, e2: Expr) -> Expr {
    Expr::Eq(Box::new(e1), Box::new(e2))
}

pub fn le(e1: Expr, e2: Expr) -> Expr {
    Expr::Le(Box::new(e1), Box::new(e2))
}

pub fn ite(e1: Expr, e2: Expr, e3: Expr) -> Expr {
    Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))
}

pub fn lets(xes: Vec<(String, Expr)>, e: Expr) -> Expr {
    let mut res = e;
    for (x, e) in xes.into_iter().rev() {
        res = Expr::Let(x, Box::new(e), Box::new(res))
    }
    res
}

pub fn set(x: &str, e: Expr) -> Expr {
    Expr::Set(x.to_string(), Box::new(e))
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(x)), e] => (x.to_string(), parse_expr(e)),
            _ => panic!("parse error"),
        },
        _ => panic!("parse error"),
    }
}

fn parse_ident(s: &Sexp) -> String {
    match s {
        Sexp::Atom(S(x)) => x.to_string(),
        _ => panic!("parse error"),
    }
}

pub fn parse_defn(s: &Sexp) -> Expr {
    let Sexp::List(es) = s else {
        panic!("syntax error: expected a list")
    };
    match &es[..] {
        [Sexp::Atom(S(op)), Sexp::List(xs), body] if op == "defn" => {
            let [name, params @ ..] = &xs[..] else {
                panic!("missing function name");
            };
            let body = Box::new(parse_expr(body));
            let name = parse_ident(name);
            let params = params.iter().map(parse_ident).collect();
            Expr::Fun(Defn {
                name: Some(name),
                params,
                body,
            })
        }
        [Sexp::Atom(S(op)), Sexp::List(xs), body] if op == "fn" => {
            let [params @ ..] = &xs[..];
            let body = Box::new(parse_expr(body));
            let params = params.iter().map(parse_ident).collect();
            Expr::Fun(Defn {
                name: None,
                params,
                body,
            })
        }

        _ => panic!("syntax error: expected a list of 4 elements: {s}"),
    }
}

fn parse_prog(e: &Sexp) -> Expr {
    let Sexp::List(es) = e else {
        panic!("syntax error: expected a list")
    };

    if let [defs @ .., expr] = &es[..] {
        let defs = defs.iter().map(|e| parse_defn(e)).collect();
        let expr = Box::new(parse_expr(expr));
        prog(defs, expr)
    } else {
        panic!("syntax error: program must contain a main expression")
    }
}

fn prog(defs: Vec<Expr>, expr: Box<Expr>) -> Expr {
    let mut res = *expr;
    for def in defs.into_iter().rev() {
        if let Expr::Fun(Defn { name, .. }) = &def {
            res = Expr::Let(name.clone().unwrap(), Box::new(def), Box::new(res));
        } else {
            panic!("syntax error: expected a function definition")
        }
    }
    res
}

fn parse_index(s: &Sexp) -> Index {
    match s {
        Sexp::Atom(I(0)) => Index::Zero,
        Sexp::Atom(I(1)) => Index::One,
        _ => panic!("parse error: {s}"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => num(i32::try_from(*n).unwrap()),

        Sexp::Atom(S(s)) if s == "input" => Expr::Input,
        Sexp::Atom(S(s)) if s == "true" => Expr::True,
        Sexp::Atom(S(s)) if s == "false" => Expr::False,

        Sexp::Atom(S(s)) => Expr::Var(s.clone()),
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => add1(parse_expr(e)),
            [Sexp::Atom(S(op)), e] if op == "sub1" => sub1(parse_expr(e)),
            [Sexp::Atom(S(op)), e] if op == "negate" => negate(parse_expr(e)),
            [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => plus(parse_expr(e1), parse_expr(e2)),
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => mult(parse_expr(e1), parse_expr(e2)),
            [Sexp::Atom(S(op)), e1, e2] if op == "=" => eq(parse_expr(e1), parse_expr(e2)),
            [Sexp::Atom(S(op)), e1, e2] if op == "<=" => le(parse_expr(e1), parse_expr(e2)),
            [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => {
                ite(parse_expr(e1), parse_expr(e2), parse_expr(e3))
            }
            [Sexp::Atom(S(op)), bind, e2] if op == "let" => {
                let (x, e1) = parse_bind(bind);
                let e2 = parse_expr(e2);
                Expr::Let(x, Box::new(e1), Box::new(e2))
            }
            [Sexp::Atom(S(op)), Sexp::List(binds), e] if op == "let*" => {
                let xes: Vec<(String, Expr)> = binds.iter().map(parse_bind).collect();
                lets(xes, parse_expr(e))
            }

            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), e] if op == "set!" => {
                Expr::Set(x.to_string(), Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                Expr::Block(exprs.into_iter().map(parse_expr).collect())
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "vec" => {
                Expr::Vek(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "vec-get" => {
                Expr::Get(Box::new(parse_expr(e1)), parse_index(e2))
            }
            [Sexp::Atom(S(op)), _, _] if op == "defn" || op == "fn" => parse_defn(s),
            [Sexp::Atom(S(f)), exprs @ ..] => {
                Expr::Call(f.to_string(), exprs.into_iter().map(parse_expr).collect())
            }
            _ => panic!("parse error (1) {}", s),
        },
        _ => panic!("parse error (2) {}", s),
    }
}

pub fn parse(s: &str) -> Expr {
    let s = format!("({})", s);
    let s = sexp::parse(&s).unwrap_or_else(|_| panic!("invalid s-expr"));
    let e = parse_prog(&s);
    let xs = free_vars(&e);
    for x in xs {
        panic!("Unbound variable: {x}")
    }
    e
}

pub fn free_vars_defn(defn: &Defn) -> HashSet<String> {
    let mut res = free_vars(&defn.body);
    if let Some(name) = &defn.name {
        res.remove(name);
    }
    for param in &defn.params {
        res.remove(param);
    }
    res
}

pub fn free_vars(e: &Expr) -> HashSet<String> {
    match e {
        Expr::Num(_) | Expr::Input | Expr::True | Expr::False => HashSet::new(),
        Expr::Var(x) => im::hashset! {x.clone()},
        Expr::Fun(defn) => free_vars_defn(defn),
        Expr::Add1(e)
        | Expr::Sub1(e)
        | Expr::Neg(e)
        | Expr::Set(_, e)
        | Expr::Loop(e)
        | Expr::Break(e)
        | Expr::Print(e)
        | Expr::Get(e, _) => free_vars(e),
        Expr::Let(x, e1, e2) => free_vars(e1).union(free_vars(e2).without(x)),
        Expr::Eq(e1, e2)
        | Expr::Le(e1, e2)
        | Expr::Plus(e1, e2)
        | Expr::Mult(e1, e2)
        | Expr::Vek(e1, e2) => free_vars(e1).union(free_vars(e2)),
        Expr::If(e1, e2, e3) => free_vars(e1).union(free_vars(e2)).union(free_vars(e3)),
        Expr::Block(es) => {
            let mut res = HashSet::new();
            for e in es {
                res = res.union(free_vars(e))
            }
            res
        }
        Expr::Call(f, es) => {
            let mut res = im::hashset! { f.clone() };
            for e in es {
                res = res.union(free_vars(e))
            }
            res
        }
    }
}
