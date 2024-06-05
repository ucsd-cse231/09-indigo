use std::env;
use std::fs::File;
use std::io::prelude::*;

use alloc::Alloc;
use expr::{Arg, Defn, Expr, Loc, Reg};
use im::{hashset, HashSet};

use crate::alloc::reg_alloc;
use crate::expr::{free_vars_defn, REGISTERS};

pub mod alloc;
pub mod anf;
pub mod expr;

const FALSE: usize = 3;
const TRUE: usize = 7;
const HEAP_REG: Reg = Reg::R11;
const INPUT_LOC: Loc = Loc::Stack(1);

const NUM_REGISTERS: usize = 8;
const _TEST_NUMBER: bool = false;

fn regs(num_regs: usize) -> Vec<Reg> {
    REGISTERS[0..num_regs].to_vec()
}

fn label(prefix: String, count: &i32) -> String {
    format!("{prefix}_{count}")
}

fn _test_number(code: usize) -> String {
    if _TEST_NUMBER {
        format!(
            "mov rcx, rax
             and rcx, 1
             cmp rcx, 0
             mov rdi, {code}
             jne label_error"
        )
    } else {
        format!("")
    }
}

fn test_closure(code: usize) -> String {
    format!(
        "mov rcx, rax
         and rcx, 5
         cmp rcx, 5
         mov rdi, {code}
         jne label_error"
    )
}

fn test_arity(arity: usize) -> String {
    format!(
        "mov rcx, rax
         sub rcx, 5         ; remove tag
         mov rcx, [rcx + 8] ; load closure-arity into rcx
         cmp rcx, {arity}   ; compare with actual #args
         mov rdi, 199       ; mismatched args error
         jne label_error"
    )
}

fn var_to_arg(env: &Alloc, x: &str) -> Arg {
    Arg::Var(env.lookup_var(x), Some(x.to_string()))
}

fn imm_to_arg(i: &Expr, env: &Alloc) -> Arg {
    match i {
        Expr::Num(n) => Arg::Con(*n << 1),
        Expr::True => Arg::Con(TRUE as i32),
        Expr::False => Arg::Con(FALSE as i32),
        Expr::Var(x) => var_to_arg(env, x),
        Expr::Input => Arg::Var(INPUT_LOC, Some("input".to_string())),
        _ => panic!("Expected immediate, got {:?}", i),
    }
}

fn loc_to_reg(loc: &Loc) -> Reg {
    match loc {
        Loc::Reg(r) => *r,
        _ => Reg::RAX,
    }
}

fn mov(dst: &Loc, reg: &Reg) -> String {
    if *dst == Loc::Reg(*reg) {
        format!("")
    } else {
        format!("mov {dst}, {reg}")
    }
}

fn mov_arg(dst: &Loc, arg: &Arg) -> String {
    if let Arg::Var(Loc::Reg(reg), _) = arg {
        if *dst == Loc::Reg(*reg) {
            return format!("");
        }
    }
    format!("mov {dst}, {arg}")
}

#[allow(dead_code)]
fn compile_imm_into(imm: &Expr, env: &Alloc, dst: &Loc) -> String {
    let imm = imm_to_arg(imm, env);
    let reg = loc_to_reg(dst);
    let mov_imm_to_reg = mov_arg(&Loc::Reg(reg), &imm);
    let mov_reg_to_dst = mov(dst, &reg);
    format!(
        "{mov_imm_to_reg}
         {mov_reg_to_dst}"
    )
}

fn compile_op1(imm: &Expr, env: &Alloc, dst: &Loc, op: impl Fn(&Reg) -> String) -> String {
    let imm = imm_to_arg(imm, env);
    let reg = loc_to_reg(dst);
    let mov_imm_to_reg = mov_arg(&Loc::Reg(reg), &imm);
    let op_reg = op(&reg);
    let mov_reg_to_dst = mov(dst, &reg);
    format!(
        "{mov_imm_to_reg}
         {op_reg}
         {mov_reg_to_dst}"
    )
}

fn compile_expr(e: &Expr, env: &Alloc, count: &mut i32, brk: &str, dst: &Loc) -> String {
    match e {
        Expr::Num(_) | Expr::True | Expr::False | Expr::Var(_) | Expr::Input => {
            todo!("fill this in")
        }
        Expr::Add1(_i) => {
            todo!("fill this in")
        }
        Expr::Sub1(_i) => {
            todo!("fill this in")
        }
        Expr::Neg(_i) => {
            todo!("fill this in")
        }
        Expr::Print(i) => compile_op1(i, env, dst, |reg| {
            format!(
                "mov rdi, {reg}
                 push {HEAP_REG}
                 push rdi
                 call snek_print
                 pop rdi
                 pop {HEAP_REG}"
            )
        }),
        Expr::Plus(_i1, _i2) => {
            todo!("fill this in")
        }
        Expr::Mult(_i1, _i2) => {
            todo!("fill this in")
        }
        Expr::Vek(i1, i2) => {
            *count += 1;
            let alloc_label = label("allocate".to_string(), count);
            let arg1 = imm_to_arg(i1, env);
            let arg2 = imm_to_arg(i2, env);
            let args: Vec<Arg> = vec![arg1, arg2];
            let alloc_code = tuple_alloc(&args);
            let mov_rax_to_dst = mov(dst, &Reg::RAX);
            format!(
                ";; allocate (vec {i1:?} {i2:?})
                 {alloc_label}:
                 {alloc_code}
                 add rax, 0x1
                 {mov_rax_to_dst}"
            )
        }
        Expr::Eq(i1, i2) => {
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            let imm1 = imm_to_arg(i1, env);
            let imm2 = imm_to_arg(i2, env);
            format!(
                "mov rax, {imm1}
                 cmp rax, {imm2}
                 mov {dst}, {FALSE}
                 jne {exit}
                 mov {dst}, {TRUE}
               {exit}:"
            )
        }
        Expr::Le(i1, i2) => {
            *count += 1;
            let exit = label("eq_exit".to_string(), count);
            let imm1 = imm_to_arg(i1, env);
            let imm2 = imm_to_arg(i2, env);
            format!(
                "mov rax, {imm2}
                 cmp rax, {imm1}
                 mov {dst}, {FALSE}
                 jl {exit}
                 mov {dst}, {TRUE}
               {exit}:
                "
            )
        }
        Expr::Let(_x, _e1, _e2) => {
            todo!("fill this in")
        }
        Expr::If(e_cond, e_then, e_else) => {
            *count += 1;
            let e_cond_code = compile_expr(e_cond, env, count, brk, &Loc::Reg(Reg::RAX));
            let e_then_code = compile_expr(e_then, env, count, brk, dst);
            let e_else_code = compile_expr(e_else, env, count, brk, dst);
            format!(
                "  {e_cond_code}
                   cmp rax, {FALSE}
                   je label_else_{count}
                   {e_then_code}
                   jmp label_exit_{count}
                 label_else_{count}:
                   {e_else_code}
                 label_exit_{count}:"
            )
        }
        Expr::Set(x, e) => {
            let x_loc = env.lookup_var(x);
            let e_code = compile_expr(e, env, count, brk, &x_loc);
            format!("{e_code}")
        }
        Expr::Block(es) => {
            let n = es.len();
            let e_codes: Vec<String> = es
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    compile_expr(
                        e,
                        env,
                        count,
                        brk,
                        if i == n - 1 { dst } else { &Loc::Reg(Reg::RAX) },
                    )
                })
                .collect();
            e_codes.join("\n")
        }
        Expr::Loop(e) => {
            *count += 1;
            let loop_start = label("loop_start".to_string(), count);
            let loop_exit = label("loop_exit".to_string(), count);
            let e_code = compile_expr(e, env, count, &loop_exit, dst);
            format!(
                "{loop_start}:
                    {e_code}
                    jmp {loop_start}
                 {loop_exit}:"
            )
        }
        Expr::Break(e) => {
            let e_code = compile_expr(e, env, count, brk, dst);
            format!(
                "{e_code}
                 jmp {brk}"
            )
        }

        Expr::Get(e, idx) => {
            let e_code = compile_expr(e, env, count, brk, &Loc::Reg(Reg::RAX));
            let tuple_read = tuple_read(1, idx.val());
            let mov_rax_to_dst = mov(dst, &Reg::RAX);
            format!(
                "{e_code}
                 {tuple_read}
                 {mov_rax_to_dst}",
            )
        }
        Expr::Call(f, is) => {
            let args: Vec<Arg> = is.iter().map(|i| imm_to_arg(i, env)).collect();
            let push_args = push_args(&args);
            let f_arg = var_to_arg(env, f);
            let test_closure = test_closure(299);
            let test_arity = test_arity(is.len());
            let pop_args = format!("add rsp, 8*{}", is.len());

            let mov_rax_to_dst = mov(dst, &Reg::RAX);
            format!(
                "{push_args}
                 mov rax, {f_arg}
                 {test_closure}
                 {test_arity}
                 push rax
                 sub rax, 5
                 call [rax]
                 {pop_args}
                 {mov_rax_to_dst}"
            )
        }
        Expr::Fun(defn) => compile_defn(defn, env, count, dst),
    }
}

fn tuple_read(tag: usize, index: usize) -> String {
    format!(
        ";; TODO: check rax is pointer
         sub rax, {tag}             ; strip tag
         mov rax, [rax + 8*{index}]"
    )
}

fn tuple_alloc(args: &Vec<Arg>) -> String {
    let mut res: Vec<String> = vec![];
    for (i, arg) in args.iter().enumerate() {
        let load_rcx = match arg {
            Arg::Con(n) => format!("mov rcx, {n}"),
            Arg::Var(loc, Some(z)) => format!("mov rcx, {loc}       ;; save `{z}` in tuple"),
            Arg::Var(loc, _) => format!("mov rcx, {loc}"),
            Arg::Lbl(label) => format!("lea rcx, QWORD [rel {label}]"),
        };
        res.push(format!(
            "{load_rcx}
             mov [{HEAP_REG} + 8*{i}], rcx",
        ));
    }
    res.push(format!(
        "mov rax, {HEAP_REG}
         add {HEAP_REG}, 8*{}",
        args.len()
    ));
    res.join("\n")
}

fn push_args(args: &[Arg]) -> String {
    let mut res: Vec<String> = vec![];
    for arg in args.iter().rev() {
        let push_arg = format!(
            "mov rcx, {arg}
             push rcx",
        );
        res.push(push_arg);
    }
    res.join("\n")
}

fn compile_exit() -> String {
    format!(
        "mov rsp, rbp
         pop rbp
         ret"
    )
}

fn compile_entry(env: &Alloc) -> String {
    let stack_size = env.stack_size();
    let size = stack_size + 10;
    format!(
        "push rbp
         mov rbp, rsp
         sub rsp, 8*{size}"
    )
}

fn defn_name(def: &Defn, count: &mut i32) -> String {
    match def.name {
        Some(ref name) => name.clone(),
        None => {
            *count += 1;
            format!("anon_{count}")
        }
    }
}

fn restore_free_vars(xs: &HashSet<String>, env: &Alloc) -> String {
    let mut res: Vec<String> = vec![];
    for (i, x) in xs.iter().enumerate() {
        let read_x = tuple_read(5, i + 2);
        let x_loc = env.lookup_var(x);
        res.push(format!(
            "mov rax, [rbp + 16]         ; load closure into rax
             {read_x}                    ; read `{x}` value ...
             mov {x_loc}, rax            ; ... restore `{x}` to stack"
        ))
    }
    res.join("\n")
}

fn save_used_regs(env: &Alloc) -> String {
    let res: Vec<String> = env
        .used_regs()
        .iter()
        .enumerate()
        .map(|(i, reg)| mov(&Loc::Stack((i + 1) as i32), reg))
        .collect();
    res.join("\n")
}

fn restore_used_regs(env: &Alloc) -> String {
    let res: Vec<String> = env
        .used_regs()
        .iter()
        .enumerate()
        .map(|(i, reg)| mov_arg(&Loc::Reg(*reg), &Arg::Var(Loc::Stack((i + 1) as i32), None)))
        .collect();
    res.join("\n")
}

fn compile_defn(defn: &Defn, env: &Alloc, count: &mut i32, dst: &Loc) -> String {
    let f = &defn_name(defn, count);
    let exit_label = format!("fun_exit_{f}");
    let free_xs = free_vars_defn(defn);
    let num_regs = env.num_regs();
    let body_env = reg_alloc(defn, &free_xs, &regs(num_regs), 0);
    let fun_entry = compile_entry(&body_env);

    let save_used_regs: String = save_used_regs(&body_env);

    let restore_free_vars = restore_free_vars(&free_xs, &body_env);
    let body_code = compile_expr(
        &defn.body,
        &body_env,
        count,
        &exit_label,
        &Loc::Reg(Reg::RAX),
    );
    let restore_used_regs: String = restore_used_regs(&body_env);
    let fun_exit = compile_exit();
    let label = format!("fun_start_{f}");
    let mut closure_args = vec![Arg::Lbl(label), Arg::Con(defn.params.len() as i32)];
    for x in free_xs {
        closure_args.push(var_to_arg(env, &x))
    }
    let alloc_closure_tuple = tuple_alloc(&closure_args);
    let mov_rax_to_dst = mov(dst, &Reg::RAX);
    format!(
        ";; BEGIN definition of {f}
         jmp fun_finish_{f}
         fun_start_{f}:
           {fun_entry}
           {save_used_regs}
         fun_body_{f}:
           {restore_free_vars}
           {body_code}
         fun_exit_{f}:
           {restore_used_regs}
           {fun_exit}
         fun_finish_{f}:
           {alloc_closure_tuple}
           add rax, 5
           {mov_rax_to_dst}
        ;; END definition of {f}"
    )
}

fn compile_prog(prog: &Expr, num_regs: usize) -> String {
    let mut count = 0;
    let defn = Defn {
        name: Some("main".to_string()),
        params: vec![],
        body: Box::new(prog.clone()),
    };
    let env = reg_alloc(&defn, &hashset! {}, &regs(num_regs), 1);
    let e_entry = compile_entry(&env);
    let e_code = compile_expr(prog, &env, &mut count, "time_to_exit", &Loc::Reg(Reg::RAX));
    let e_exit = compile_exit();
    format!(
        "section .text
global our_code_starts_here
extern snek_error
extern snek_print
label_error:
  push rsp
  call snek_error
our_code_starts_here:
 {e_entry}
 mov {INPUT_LOC}, rdi       ;; save input in INPUT_LOC
 mov {HEAP_REG}, rsi        ;; save start of heap in HEAP_REG
 {e_code}
 {e_exit}
time_to_exit:
  ret
"
    )
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = expr::parse(&in_contents);
    let prog = anf::anf(&prog);

    let num_regs = match env::var("NUM_REGS") {
        Ok(s) => s.parse().unwrap(),
        Err(_) => NUM_REGISTERS,
    };

    let mut out_file = File::create(out_name)?;
    let asm_program = compile_prog(&prog, num_regs);
    let asm_program = trim_string(asm_program);
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

fn trim_line(s: &str) -> Option<String> {
    let s = s.trim().to_string();
    if s.is_empty() {
        None
    } else if s.ends_with(':') {
        Some(s)
    } else {
        Some(format!("   {}", s))
    }
}

fn trim_string(s: String) -> String {
    let lines: Vec<String> = s.split('\n').filter_map(trim_line).collect();
    lines.join("\n")
}
