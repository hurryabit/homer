use crate::*;
use std::fmt;
use syntax::debug::{Debug, DebugWriter};

use location::{ParserLoc, Span};
use syntax::{ExprCon, ExprVar, OpCode};

#[derive(Clone, Eq, PartialEq)]
pub struct Module {
    pub func_decls: Vec<FuncDecl>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct FuncDecl {
    pub name: ExprVar,
    pub params: Vec<ExprVar>,
    pub body: Expr,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Binding {
    pub binder: ExprVar,
    pub bindee: Bindee,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Atom(ExprVar);

#[derive(Clone, Eq, PartialEq)]
pub enum Bindee {
    Error(Span<ParserLoc>),
    Var(ExprVar),
    Num(i64),
    Bool(bool),
    Lam(Vec<ExprVar>, Box<Expr>),
    AppClos(ExprVar, Vec<Atom>),
    AppFunc(ExprVar, Vec<Atom>),
    BinOp(Atom, OpCode, Atom),
    If(Atom, Box<Expr>, Box<Expr>),
    Record(Vec<ExprVar>, Vec<Atom>),
    Proj(Atom, ExprVar),
    Variant(ExprCon, Option<Atom>),
    Match(Atom, Vec<Branch>),
}

pub type Body = Bindee;

#[derive(Clone, Eq, PartialEq)]
pub struct Branch {
    pub pattern: Pattern,
    pub expr: Expr,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Pattern {
    pub constr: ExprCon,
    pub binder: Option<ExprVar>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Expr {
    pub bindings: Vec<Binding>,
    pub body: Body,
}

impl syntax::Module {
    pub fn to_anf(&self) -> Module {
        let func_decls = self.func_decls().map(|decl| decl.to_anf()).collect();
        Module { func_decls }
    }
}

impl syntax::FuncDecl {
    fn to_anf(&self) -> FuncDecl {
        let Self {
            name,
            type_params: _,
            expr_params,
            return_type: _,
            body,
        } = self;
        let name = name.locatee;
        let params: Vec<_> = expr_params.iter().map(|(param, _)| param.locatee).collect();
        let env = &mut Env {
            bound: params.iter().copied().collect(),
            renaming: im::HashMap::new(),
        };
        let body = Expr::from_syntax(env, body);
        FuncDecl { name, params, body }
    }
}

impl Expr {
    fn from_syntax(env: &mut Env, expr: &syntax::LExpr) -> Self {
        let mut bindings = Vec::new();
        let body = Bindee::from_syntax(env, expr, &mut bindings);
        Self { bindings, body }
    }
}

impl Bindee {
    fn from_syntax(env: &mut Env, expr: &syntax::LExpr, bindings: &mut Vec<Binding>) -> Self {
        match &expr.locatee {
            syntax::Expr::Error => Self::Error(expr.span),
            syntax::Expr::Var(var) => Self::Var(*env.get_binder(var)),
            syntax::Expr::Num(n) => Self::Num(*n),
            syntax::Expr::Bool(b) => Self::Bool(*b),
            syntax::Expr::Lam(params, body) => {
                let env = &mut env.clone();
                let params = params
                    .iter()
                    .map(|(param, _)| env.intro_binder(param))
                    .collect();
                let body = Expr::from_syntax(env, body);
                Self::Lam(params, Box::new(body))
            }
            syntax::Expr::App(fun, args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| Atom::from_syntax(env, arg, bindings))
                    .collect();
                match &fun.locatee {
                    syntax::Expr::Var(var) => Self::AppClos(*env.get_binder(var), args),
                    syntax::Expr::FuncInst(func, _) => Self::AppFunc(func.locatee, args),
                    expr => panic!("Application of unnamed function: {:?}", expr),
                }
            }
            expr @ syntax::Expr::FuncInst(..) => {
                panic!("Standalone function instantiation: {:?}", expr)
            }
            syntax::Expr::BinOp(lhs, op, rhs) => {
                let lhs = Atom::from_syntax(env, lhs, bindings);
                let rhs = Atom::from_syntax(env, rhs, bindings);
                Bindee::BinOp(lhs, *op, rhs)
            }
            syntax::Expr::Let(binder, _, bindee, body) => {
                let bindee = Self::from_syntax(env, bindee, bindings);
                let binder = env.intro_binder(binder);
                bindings.push(Binding { binder, bindee });
                Self::from_syntax(env, body, bindings)
            }
            syntax::Expr::If(cond, then, elze) => {
                let cond = Atom::from_syntax(env, cond, bindings);
                let then = Expr::from_syntax(&mut env.clone(), then);
                let elze = Expr::from_syntax(&mut env.clone(), elze);
                Self::If(cond, Box::new(then), Box::new(elze))
            }
            syntax::Expr::Record(fields_exprs) => {
                let (fields, exprs) = fields_exprs
                    .iter()
                    .map(|(field, expr)| (field.locatee, Atom::from_syntax(env, expr, bindings)))
                    .unzip();
                Self::Record(fields, exprs)
            }
            syntax::Expr::Proj(record, field) => {
                let record = Atom::from_syntax(env, record, bindings);
                Self::Proj(record, field.locatee)
            }
            syntax::Expr::Variant(constr, opt_payload) => {
                let payload = opt_payload
                    .as_ref()
                    .map(|payload| Atom::from_syntax(env, &payload, bindings));
                Self::Variant(*constr, payload)
            }
            syntax::Expr::Match(scrut, branches) => {
                let scrut = Atom::from_syntax(env, scrut, bindings);
                let branches = branches
                    .iter()
                    .map(|branch| Branch::from_syntax(&mut env.clone(), branch))
                    .collect();
                Self::Match(scrut, branches)
            }
        }
    }
}

impl Pattern {
    fn from_syntax(env: &mut Env, pattern: &syntax::LPattern) -> Self {
        let syntax::Pattern { constr, binder } = pattern.locatee;
        let binder = binder.as_ref().map(|binder| env.intro_binder(binder));
        Self { constr, binder }
    }
}

impl Branch {
    fn from_syntax(env: &mut Env, branch: &syntax::Branch) -> Branch {
        let syntax::Branch { pattern, body } = branch;
        let pattern = Pattern::from_syntax(env, pattern);
        let expr = Expr::from_syntax(env, body);
        Self { pattern, expr }
    }
}

impl Atom {
    fn from_syntax(env: &mut Env, expr: &syntax::LExpr, bindings: &mut Vec<Binding>) -> Self {
        if let syntax::Expr::Var(var) = expr.locatee {
            Self(var)
        } else {
            let bindee = Bindee::from_syntax(env, expr, bindings);
            let binder = env.intro_fresh_binder();
            bindings.push(Binding { binder, bindee });
            Self(binder)
        }
    }
}

#[derive(Clone)]
struct Env {
    bound: im::HashSet<ExprVar>,
    renaming: im::HashMap<ExprVar, ExprVar>,
}

impl Env {
    fn intro_fresh_binder(&mut self) -> ExprVar {
        let mut n: usize = 1;
        loop {
            let var = ExprVar::new(&format!("$v{}", n));
            if !self.bound.contains(&var) {
                self.bound.insert(var);
                return var;
            }
            n += 1;
        }
    }

    fn intro_binder(&mut self, binder: &syntax::LExprVar) -> ExprVar {
        let binder = binder.locatee;
        if self.bound.contains(&binder) {
            let var = self.intro_fresh_binder();
            self.renaming.insert(binder, var);
            var
        } else {
            self.bound.insert(binder);
            binder
        }
    }

    fn get_binder<'a>(&'a self, var: &'a ExprVar) -> &'a ExprVar {
        self.renaming.get(var).unwrap_or(var)
    }
}

derive_debug!(Module);

impl Debug for Module {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { func_decls } = self;
        writer.node("MODULE", |writer| {
            for decl in func_decls {
                writer.child("decl", decl)?;
            }
            Ok(())
        })
    }
}

impl Debug for FuncDecl {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { name, params, body } = self;
        writer.node("FUNCDECL", |writer| {
            writer.child("name", name)?;
            for param in params {
                writer.child("param", param)?;
            }
            writer.child("body", body)
        })
    }
}

impl Debug for Expr {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { bindings, body } = self;
        writer.node("EXPR", |writer| {
            for Binding { binder, bindee } in bindings {
                writer.child("binder", binder)?;
                writer.child("bindee", bindee)?;
            }
            writer.child("body", body)
        })
    }
}

impl Debug for Bindee {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        use Bindee::*;
        match self {
            Error(_) => writer.leaf("ERROR"),
            Var(name) => name.write(writer),
            Num(n) => writer.leaf(&n.to_string()),
            Bool(b) => writer.leaf(&b.to_string()),
            Lam(params, body) => writer.node("LAM", |writer| {
                for param in params {
                    writer.child("param", param)?;
                }
                writer.child("body", body)
            }),
            AppClos(fun, args) | AppFunc(fun, args) => writer.node("APP", |writer| {
                writer.child("fun", fun)?;
                for arg in args {
                    writer.child("arg", arg)?;
                }
                Ok(())
            }),
            BinOp(lhs, op, rhs) => writer.node("BINOP", |writer| {
                writer.child("lhs", lhs)?;
                writer.child("op", op)?;
                writer.child("rhs", rhs)
            }),
            If(cond, then, elze) => writer.node("IF", |writer| {
                writer.child("cond", cond)?;
                writer.child("then", then)?;
                writer.child("else", elze)
            }),
            Record(fields, exprs) => writer.node("RECORD", |writer| {
                for (field, value) in fields.iter().zip(exprs.iter()) {
                    writer.child("field", field)?;
                    writer.child("value", value)?;
                }
                Ok(())
            }),
            Proj(record, field) => writer.node("PROJ", |writer| {
                writer.child("record", record)?;
                writer.child("field", field)
            }),
            Variant(constr, opt_payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", constr)?;
                if let Some(payload) = opt_payload {
                    writer.child("payload", payload)?;
                }
                Ok(())
            }),
            Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                for branch in branches {
                    writer.child("branch", branch)?;
                }
                Ok(())
            }),
        }
    }
}

impl Debug for Atom {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        self.0.write(writer)
    }
}

impl Debug for Branch {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { pattern, expr } = self;
        writer.node("BRANCH", |writer| {
            writer.child("pattern", pattern)?;
            writer.child("body", expr)
        })
    }
}

impl Debug for Pattern {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { constr, binder } = self;
        writer.node("PATTERN", |writer| {
            writer.child("constr", constr)?;
            if let Some(binder) = binder {
                writer.child("binder", binder)?;
            }
            Ok(())
        })
    }
}
