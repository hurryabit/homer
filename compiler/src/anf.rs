use crate::*;
use join_lazy_fmt::*;
use std::fmt;
use syntax::debug::{Debug, DebugWriter};
use util::in_parens_if_some;

use location::{ParserLoc, Span};
pub use syntax::{ExprCon, ExprVar, OpCode};

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
pub struct Expr {
    pub bindings: Vec<Binding>,
    pub body: Body,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Binding {
    pub binder: ExprVar,
    pub bindee: Bindee,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Atom(pub ExprVar);

#[derive(Clone, Eq, PartialEq)]
pub enum Bindee {
    Error(Span<ParserLoc>),
    Atom(Atom),
    Num(i64),
    Bool(bool),
    Clos {
        captured: Vec<ExprVar>,
        params: Vec<ExprVar>,
        body: Box<Expr>,
    },
    AppClos(Atom, Vec<Atom>),
    AppFunc(ExprVar, Vec<Atom>),
    BinOp(Atom, OpCode, Atom),
    If(Atom, Box<Expr>, Box<Expr>),
    Record(Vec<(ExprVar, Atom)>),
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
        let env = &mut Env::default();
        let params: Vec<_> = expr_params
            .iter()
            .map(|(param, _)| env.intro_binder(param))
            .collect();
        let (body, _fvs) = Expr::from_syntax(env, body);
        FuncDecl { name, params, body }
    }
}

type FreeVars = im::OrdSet<ExprVar>;

impl Expr {
    fn from_syntax(env: &mut Env, expr: &syntax::LExpr) -> (Self, FreeVars) {
        let mut bindings = Vec::new();
        let (body, fvs) = Bindee::from_syntax(env, expr, &mut bindings);
        (Self { bindings, body }, fvs)
    }
}

impl Bindee {
    fn from_syntax(
        env: &mut Env,
        expr: &syntax::LExpr,
        bindings: &mut Vec<Binding>,
    ) -> (Self, FreeVars) {
        match &expr.locatee {
            syntax::Expr::Error => (Self::Error(expr.span), ordset![]),
            syntax::Expr::Var(_) => {
                let (atom, fvs) = Atom::from_syntax(env, expr, bindings);
                (Self::Atom(atom), fvs)
            }
            syntax::Expr::Num(n) => (Self::Num(*n), ordset![]),
            syntax::Expr::Bool(b) => (Self::Bool(*b), ordset![]),
            syntax::Expr::Lam(params, body) => {
                let (params, body, fvs) = {
                    let env = &mut env.clone();
                    let params: Vec<_> = params
                        .iter()
                        .map(|(param, _)| env.intro_binder(param))
                        .collect();
                    let (body, fvs) = Expr::from_syntax(env, body);
                    let fvs = params.iter().fold(fvs, |fvs, param| fvs.without(param));
                    (params, Box::new(body), fvs)
                };
                let mut captured: Vec<_> = fvs.iter().copied().collect();
                captured.sort_by_cached_key(|fv| env.get_index(fv));
                (
                    Self::Clos {
                        captured,
                        params,
                        body,
                    },
                    fvs,
                )
            }
            syntax::Expr::App(fun, args) => {
                let (args, fvss): (_, Vec<_>) = args
                    .iter()
                    .map(|arg| Atom::from_syntax(env, arg, bindings))
                    .unzip();
                let fvs = FreeVars::unions(fvss);
                if let syntax::Expr::FuncInst(func, _) = &fun.locatee {
                    (Self::AppFunc(func.locatee, args), fvs)
                } else {
                    let (atom, fvs1) = Atom::from_syntax(env, fun, bindings);
                    (Self::AppClos(atom, args), fvs.union(fvs1))
                }
            }
            expr @ syntax::Expr::FuncInst(..) => {
                panic!("Standalone function instantiation: {:?}", expr)
            }
            syntax::Expr::BinOp(lhs, op, rhs) => {
                let (lhs, fvs1) = Atom::from_syntax(env, lhs, bindings);
                let (rhs, fvs2) = Atom::from_syntax(env, rhs, bindings);
                (Bindee::BinOp(lhs, *op, rhs), fvs1.union(fvs2))
            }
            syntax::Expr::Let(binder, _, bindee, body) => {
                let (bindee, fvs1) = Self::from_syntax(env, bindee, bindings);
                let binder = env.intro_binder(binder);
                bindings.push(Binding { binder, bindee });
                let (body, fvs2) = Self::from_syntax(env, body, bindings);
                (body, fvs1.union(fvs2.without(&binder)))
            }
            syntax::Expr::If(cond, then, elze) => {
                let (cond, fvs1) = Atom::from_syntax(env, cond, bindings);
                let (then, fvs2) = Expr::from_syntax(&mut env.clone(), then);
                let (elze, fvs3) = Expr::from_syntax(&mut env.clone(), elze);
                let fvs = fvs1.union(fvs2).union(fvs3);
                (Self::If(cond, Box::new(then), Box::new(elze)), fvs)
            }
            syntax::Expr::Record(fields) => {
                let (fields, fvss): (_, Vec<_>) = fields
                    .iter()
                    .map(|(field, expr)| {
                        let (expr, fvs) = Atom::from_syntax(env, expr, bindings);
                        ((field.locatee, expr), fvs)
                    })
                    .unzip();
                (Self::Record(fields), FreeVars::unions(fvss))
            }
            syntax::Expr::Proj(record, field) => {
                let (record, fvs) = Atom::from_syntax(env, record, bindings);
                (Self::Proj(record, field.locatee), fvs)
            }
            syntax::Expr::Variant(constr, None) => (Self::Variant(*constr, None), ordset![]),
            syntax::Expr::Variant(constr, Some(payload)) => {
                let (payload, fvs) = Atom::from_syntax(env, payload, bindings);
                (Self::Variant(*constr, Some(payload)), fvs)
            }
            syntax::Expr::Match(scrut, branches) => {
                let (scrut, fvs0) = Atom::from_syntax(env, scrut, bindings);
                let (branches, fvss): (_, Vec<_>) = branches
                    .iter()
                    .map(|branch| Branch::from_syntax(&mut env.clone(), branch))
                    .unzip();
                (
                    Self::Match(scrut, branches),
                    fvs0.union(FreeVars::unions(fvss)),
                )
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
    fn from_syntax(env: &mut Env, branch: &syntax::Branch) -> (Branch, FreeVars) {
        let syntax::Branch { pattern, body } = branch;
        let pattern = Pattern::from_syntax(env, pattern);
        let (expr, fvs) = Expr::from_syntax(env, body);
        let fvs = if let Some(binder) = pattern.binder {
            fvs.without(&binder)
        } else {
            fvs
        };
        (Self { pattern, expr }, fvs)
    }
}

impl Atom {
    fn from_syntax(
        env: &mut Env,
        expr: &syntax::LExpr,
        bindings: &mut Vec<Binding>,
    ) -> (Self, FreeVars) {
        if let syntax::Expr::Var(var) = expr.locatee {
            let binder = *env.get_binder(&var);
            (Self(binder), ordset![binder])
        } else {
            let (bindee, fvs) = Bindee::from_syntax(env, expr, bindings);
            let binder = env.intro_fresh_binder();
            bindings.push(Binding { binder, bindee });
            (Self(binder), fvs)
        }
    }
}

#[derive(Clone, Default)]
struct Env {
    bound: im::HashMap<ExprVar, usize>,
    renaming: im::HashMap<ExprVar, ExprVar>,
}

impl Env {
    fn intro_fresh_binder(&mut self) -> ExprVar {
        let mut n: usize = 1;
        loop {
            let var = ExprVar::new(&format!("$v{}", n));
            if !self.bound.contains_key(&var) {
                let index = self.bound.len();
                self.bound.insert(var, index);
                return var;
            }
            n += 1;
        }
    }

    fn intro_binder(&mut self, binder: &syntax::LExprVar) -> ExprVar {
        let binder = binder.locatee;
        if self.bound.contains_key(&binder) {
            let var = self.intro_fresh_binder();
            self.renaming.insert(binder, var);
            var
        } else {
            let index = self.bound.len();
            self.bound.insert(binder, index);
            binder
        }
    }

    fn get_binder<'a>(&'a self, var: &'a ExprVar) -> &'a ExprVar {
        self.renaming.get(var).unwrap_or(var)
    }

    fn get_index(&self, var: &ExprVar) -> usize {
        *self.bound.get(var).unwrap()
    }
}

derive_fmt_debug!(Module);

impl Debug for Module {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { func_decls } = self;
        writer.node("MODULE", |writer| writer.children("decl", func_decls))
    }
}

impl Debug for FuncDecl {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { name, params, body } = self;
        writer.node("FUNCDECL", |writer| {
            writer.child("name", name)?;
            writer.children("param", params)?;
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
            Atom(atom) => atom.write(writer),
            Num(n) => writer.leaf(&n.to_string()),
            Bool(b) => writer.leaf(&b.to_string()),
            Clos {
                captured,
                params,
                body,
            } => writer.node("LAM", |writer| {
                writer.children("captured", captured)?;
                writer.children("param", params)?;
                writer.child("body", body)
            }),
            AppClos(fun, args) => writer.node("APP", |writer| {
                writer.child("fun", fun)?;
                writer.children("arg", args)
            }),
            AppFunc(fun, args) => writer.node("APP", |writer| {
                writer.child("fun", fun)?;
                writer.children("arg", args)
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
            Record(fields) => writer.node("RECORD", |writer| {
                writer.children_pair("field", "value", fields)
            }),
            Proj(record, field) => writer.node("PROJ", |writer| {
                writer.child("record", record)?;
                writer.child("field", field)
            }),
            Variant(constr, opt_payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", constr)?;
                writer.child_if_some("payload", opt_payload)
            }),
            Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                writer.children("branch", branches)
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
            writer.child_if_some("binder", binder)
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { bindings, body } = self;
        write!(f, "{}\n{}", "\n".join(bindings), body)
    }
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { binder, bindee } = self;
        write!(f, "let {} = {};", binder, bindee)
    }
}

impl fmt::Display for Bindee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Bindee::*;
        match self {
            Error(_) => write!(f, "???"),
            Atom(atom) => write!(f, "{}", atom),
            Num(n) => write!(f, "{}", n),
            Bool(b) => write!(f, "{}", b),
            Clos {
                captured,
                params,
                body: _,
            } => write!(f, "[{}; {}; ...]", ", ".join(captured), ", ".join(params)),
            AppClos(fun, args) => write!(f, "{}({})", fun, ", ".join(args)),
            AppFunc(fun, args) => write!(f, "{}({})", fun, ", ".join(args)),
            BinOp(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            If(cond, _, _) => write!(f, "if {} {{ .. }} else {{ .. }}", cond),
            Record(fields) => write!(
                f,
                "{{{}}}",
                ", ".join(
                    fields
                        .iter()
                        .map(|(field, value)| lazy_format!("{} = {}", field, value))
                )
            ),
            Proj(record, field) => write!(f, "{}.{}", record, field),
            Variant(constr, payload) => write!(f, "{}{}", constr, in_parens_if_some(payload)),
            Match(scrut, branches) => write!(
                f,
                "match {} {{ {}, }}",
                scrut,
                ", ".join(branches.iter().map(|branch| {
                    let Pattern { constr, binder } = branch.pattern;
                    lazy_format!("{}{} => ...", constr, in_parens_if_some(&binder))
                }))
            ),
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            OpCode::Add => "+",
            OpCode::Sub => "-",
            OpCode::Mul => "*",
            OpCode::Div => "/",
            OpCode::Equals => "==",
            OpCode::NotEq => "!=",
            OpCode::Less => "<",
            OpCode::LessEq => "<=",
            OpCode::Greater => ">",
            OpCode::GreaterEq => ">=",
        })
    }
}
