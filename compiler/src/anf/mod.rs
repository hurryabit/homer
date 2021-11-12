use crate::*;
use join_lazy_fmt::*;
use std::fmt;
use util::in_parens_if_some;

use location::SourceSpan;
pub use syntax::{ExprCon, ExprVar, LExprVar, OpCode};

mod debruijn;

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
    // NOTE(MH): This vector is never empty. The vector
    // `[(x_1, e_1), ..., (x_n, e_n)]` represents the expression
    // `let x_1 = e_1; ...; let x_{n-1} = e_{n-1}; e_n`. `x_n` is always
    // "$result" and is only there to make the presentation uniform for
    // for the sake of easier interpretation.
    pub bindings: Vec<Binding>,
}

pub type TailExpr = Bindee;

#[derive(Clone, Eq, PartialEq)]
pub struct Binding {
    pub binder: ExprVar,
    pub bindee: Bindee,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Bindee {
    Error(SourceSpan),
    Atom(Atom),
    Num(i64),
    Bool(bool),
    MakeClosure(MakeClosure),
    AppClosure(Atom, Vec<Atom>),
    AppFunc(u32, ExprVar, Vec<Atom>),
    AppExtern(ExprVar, Vec<Atom>),
    BinOp(Atom, OpCode, Atom),
    If(Atom, Box<Expr>, Box<Expr>),
    // NOTE(MH): Both vectors always have the same length. We split it here
    // because we want to be able to borrow the first half without borrowing the
    // other half.
    Record(Vec<ExprVar>, Vec<Atom>),
    Project(Atom, u32, ExprVar),
    Variant(u32, ExprCon, Option<Atom>),
    Match(Atom, Vec<Branch>),
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct IdxVar(pub u32, pub ExprVar);

#[derive(Clone, Eq, PartialEq)]
pub struct Atom(pub IdxVar);

#[derive(Clone, Eq, PartialEq)]
pub struct MakeClosure {
    pub captured: Vec<IdxVar>,
    pub params: Vec<ExprVar>,
    pub body: Box<Expr>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Branch {
    pub pattern: Pattern,
    pub rhs: Expr,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Pattern {
    pub rank: u32,
    pub constr: ExprCon,
    pub binder: Option<ExprVar>,
}

impl syntax::Module {
    pub fn to_anf(&self) -> Module {
        let func_indices = self
            .func_decls()
            .enumerate()
            .map(|(index, decl)| (decl.name.locatee, index as u32))
            .collect();
        let func_decls = self
            .func_decls()
            .filter_map(|decl| if decl.is_extern { None } else { Some(decl.to_anf(&func_indices)) })
            .collect();
        Module { func_decls }
    }
}

impl syntax::FuncDecl {
    fn to_anf(&self, func_indices: &im::HashMap<ExprVar, u32>) -> FuncDecl {
        let Self { name, type_params: _, expr_params, return_type: _, body, is_extern } = self;
        assert!(!is_extern);
        let name = name.locatee;
        let env = &mut Env::new(func_indices);
        let params: Vec<_> = expr_params.iter().map(|(param, _)| env.intro_binder(param)).collect();
        let (body, _fvs) = Expr::from_syntax(env, body);
        let mut decl = FuncDecl { name, params, body };
        decl.index();
        decl
    }
}

type FreeVars = im::OrdSet<ExprVar>;

impl Expr {
    fn from_syntax(env: &mut Env, expr: &syntax::LExpr) -> (Self, FreeVars) {
        let mut bindings = Vec::new();
        let (tail, fvs) = Bindee::from_syntax(env, expr, &mut bindings);
        bindings.push(Binding { binder: ExprVar::new("$result"), bindee: tail });
        (Self { bindings }, fvs)
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
            syntax::Expr::Var(var) => {
                let (atom, fvs) = Atom::from_var(env, var);
                (Self::Atom(atom), fvs)
            }
            syntax::Expr::Num(n) => (Self::Num(*n), ordset![]),
            syntax::Expr::Bool(b) => (Self::Bool(*b), ordset![]),
            syntax::Expr::Lam(params, body) => {
                let (params, body, fvs) = {
                    let env = &mut env.clone();
                    let params: Vec<_> =
                        params.iter().map(|(param, _)| env.intro_binder(param)).collect();
                    let (body, fvs) = Expr::from_syntax(env, body);
                    let fvs = params.iter().fold(fvs, |fvs, param| fvs.without(param));
                    (params, Box::new(body), fvs)
                };
                let mut captured: Vec<_> = fvs.iter().map(|x| IdxVar(0, *x)).collect();
                captured.sort_by_cached_key(|IdxVar(_, x)| env.get_index(x));
                (Self::MakeClosure(MakeClosure { captured, params, body }), fvs)
            }
            syntax::Expr::AppClo(clo, args) => {
                let (fun, fvs) = Atom::from_var(env, clo);
                let (args, fvss): (_, Vec<_>) =
                    args.iter().map(|arg| Atom::from_syntax(env, arg, bindings)).unzip();
                (Self::AppClosure(fun, args), fvs.union(FreeVars::unions(fvss)))
            }
            syntax::Expr::AppFun(fun, _types, args) => {
                let (args, fvss): (_, Vec<_>) =
                    args.iter().map(|arg| Atom::from_syntax(env, arg, bindings)).unzip();
                let name = fun.locatee;
                let index = env.func_indices.get(&name).unwrap();
                (Self::AppFunc(*index, name, args), FreeVars::unions(fvss))
            }
            syntax::Expr::AppExt(ext, _types, args) => {
                let (args, fvss): (_, Vec<_>) =
                    args.iter().map(|arg| Atom::from_syntax(env, arg, bindings)).unzip();
                (Self::AppExtern(ext.locatee, args), FreeVars::unions(fvss))
            }
            syntax::Expr::BinOp(lhs, op, rhs) => {
                let (lhs, fvs1) = Atom::from_syntax(env, lhs, bindings);
                let (rhs, fvs2) = Atom::from_syntax(env, rhs, bindings);
                (Bindee::BinOp(lhs, *op, rhs), fvs1.union(fvs2))
            }
            syntax::Expr::Let(binder, _, bindee, expr) => {
                let (bindee, fvs1) = Self::from_syntax(env, bindee, bindings);
                let binder = env.intro_binder(binder);
                bindings.push(Binding { binder, bindee });
                let (tail, fvs2) = Self::from_syntax(env, expr, bindings);
                (tail, fvs1.union(fvs2.without(&binder)))
            }
            syntax::Expr::If(cond, then, elze) => {
                let (cond, fvs1) = Atom::from_syntax(env, cond, bindings);
                let (then, fvs2) = Expr::from_syntax(&mut env.clone(), then);
                let (elze, fvs3) = Expr::from_syntax(&mut env.clone(), elze);
                let fvs = fvs1.union(fvs2).union(fvs3);
                (Self::If(cond, Box::new(then), Box::new(elze)), fvs)
            }
            syntax::Expr::Record(fields) => {
                let (fields, fvss): (Vec<_>, Vec<_>) = fields
                    .iter()
                    .map(|(field, expr)| {
                        let (expr, fvs) = Atom::from_syntax(env, expr, bindings);
                        ((field.locatee, expr), fvs)
                    })
                    .unzip();
                let (names, values) = fields.into_iter().unzip();
                (Self::Record(names, values), FreeVars::unions(fvss))
            }
            syntax::Expr::Proj(record, field, index) => {
                let index = index.expect("Projection without index");
                let (record, fvs) = Atom::from_syntax(env, record, bindings);
                (Self::Project(record, index, field.locatee), fvs)
            }
            syntax::Expr::Variant(constr, rank, None) => {
                let variant =
                    Self::Variant(rank.expect("Variant constructor without rank"), *constr, None);
                (variant, ordset![])
            }
            syntax::Expr::Variant(constr, rank, Some(payload)) => {
                let (payload, fvs) = Atom::from_syntax(env, payload, bindings);
                let variant = Self::Variant(
                    rank.expect("Variant constructor without rank"),
                    *constr,
                    Some(payload),
                );
                (variant, fvs)
            }
            syntax::Expr::Match(scrut, branches) => {
                let (scrut, fvs0) = Atom::from_syntax(env, scrut, bindings);
                let (mut branches, fvss): (Vec<_>, Vec<_>) = branches
                    .iter()
                    .map(|branch| Branch::from_syntax(&mut env.clone(), branch))
                    .unzip();
                branches.sort_by_key(|branch| branch.pattern.rank);
                assert!(branches
                    .iter()
                    .enumerate()
                    .all(|(rank, branch)| rank == branch.pattern.rank as usize));
                (Self::Match(scrut, branches), fvs0.union(FreeVars::unions(fvss)))
            }
        }
    }
}

impl Pattern {
    fn from_syntax(env: &mut Env, pattern: &syntax::LPattern) -> Self {
        let syntax::Pattern { constr, rank, binder } = pattern.locatee;
        let rank = rank.expect("Variant pattern without rank");
        let binder = binder.as_ref().map(|binder| env.intro_binder(binder));
        Self { rank, constr, binder }
    }
}

impl Branch {
    fn from_syntax(env: &mut Env, branch: &syntax::Branch) -> (Branch, FreeVars) {
        let syntax::Branch { pattern, rhs } = branch;
        let pattern = Pattern::from_syntax(env, pattern);
        let (expr, fvs) = Expr::from_syntax(env, rhs);
        let fvs = match pattern.binder {
            None => fvs,
            Some(binder) => fvs.without(&binder),
        };
        (Self { pattern, rhs: expr }, fvs)
    }
}

impl Atom {
    fn from_var(env: &mut Env, var: &LExprVar) -> (Self, FreeVars) {
        let binder = *env.get_binder(&var.locatee);
        (Self(IdxVar(0, binder)), ordset![binder])
    }

    fn from_syntax(
        env: &mut Env,
        expr: &syntax::LExpr,
        bindings: &mut Vec<Binding>,
    ) -> (Self, FreeVars) {
        if let syntax::Expr::Var(var) = &expr.locatee {
            Self::from_var(env, var)
        } else {
            let (bindee, fvs) = Bindee::from_syntax(env, expr, bindings);
            let binder = env.intro_fresh_binder();
            bindings.push(Binding { binder, bindee });
            (Self(IdxVar(0, binder)), fvs)
        }
    }
}

#[derive(Clone)]
struct Env {
    bound: im::HashMap<ExprVar, usize>,
    renaming: im::HashMap<ExprVar, ExprVar>,
    func_indices: im::HashMap<ExprVar, u32>,
}

impl Env {
    fn new(func_indices: &im::HashMap<ExprVar, u32>) -> Self {
        Self {
            bound: im::HashMap::new(),
            renaming: im::HashMap::new(),
            func_indices: func_indices.clone(),
        }
    }

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

impl ast::Debug for Module {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { func_decls } = self;
        writer.node("MODULE", |writer| writer.children("decl", func_decls))
    }
}

impl ast::Debug for FuncDecl {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { name, params, body } = self;
        writer.node("FUNCDECL", |writer| {
            writer.child("name", name)?;
            writer.children("param", params)?;
            writer.child("body", body)
        })
    }
}

impl ast::Debug for Expr {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { bindings } = self;
        writer.node("EXPR", |writer| {
            for Binding { binder, bindee } in bindings {
                writer.child("binder", binder)?;
                writer.child("bindee", bindee)?;
            }
            Ok(())
        })
    }
}

impl ast::Debug for Bindee {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Error(_) => writer.leaf("ERROR"),
            Self::Atom(atom) => atom.write(writer),
            Self::Num(n) => writer.leaf(&n.to_string()),
            Self::Bool(b) => writer.leaf(&b.to_string()),
            Self::MakeClosure(closure) => closure.write(writer),
            Self::AppClosure(clo, args) => writer.node("APP", |writer| {
                writer.child("fun", clo)?;
                writer.children("arg", args)
            }),
            Self::AppFunc(index, name, args) => writer.node("APP", |writer| {
                writer.child("fun", &(*name, *index))?;
                writer.children("arg", args)
            }),
            Self::AppExtern(name, args) => writer.node("APP", |writer| {
                writer.child("ext", name)?;
                writer.children("arg", args)
            }),
            Self::BinOp(lhs, op, rhs) => writer.node("BINOP", |writer| {
                writer.child("lhs", lhs)?;
                writer.child("op", op)?;
                writer.child("rhs", rhs)
            }),
            Self::If(cond, then, elze) => writer.node("IF", |writer| {
                writer.child("cond", cond)?;
                writer.child("then", then)?;
                writer.child("else", elze)
            }),
            Self::Record(fields, values) => writer.node("RECORD", |writer| {
                for (field, value) in fields.iter().zip(values.iter()) {
                    writer.child("field", field)?;
                    writer.child("value", value)?;
                }
                Ok(())
            }),
            Self::Project(record, index, field) => writer.node("PROJECT", |writer| {
                writer.child("record", record)?;
                writer.child("field", &(*field, *index))
            }),
            Self::Variant(rank, constr, payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", &(*constr, *rank))?;
                writer.child_if_some("payload", payload)
            }),
            Self::Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                writer.children("branch", branches)
            }),
        }
    }
}

impl ast::Debug for IdxVar {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        (self.1, self.0).write(writer)
    }
}
impl ast::Debug for Atom {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        self.0.write(writer)
    }
}

impl ast::Debug for MakeClosure {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let MakeClosure { captured, params, body } = self;
        writer.node("MAKE_CLOSURE", |writer| {
            writer.children("captured", captured.iter().map(|idx| idx.to_string()))?;
            writer.children("param", params)?;
            writer.child("body", body)
        })
    }
}

impl ast::Debug for Branch {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { pattern, rhs } = self;
        writer.node("BRANCH", |writer| {
            writer.child("pattern", pattern)?;
            writer.child("rhs", rhs)
        })
    }
}

impl ast::Debug for Pattern {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { rank, constr, binder } = self;
        writer.node("PATTERN", |writer| {
            writer.child("constr", &(*constr, *rank))?;
            writer.child_if_some("binder", binder)
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { bindings } = self;
        write!(f, "{}", "\n".join(bindings))
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
        match self {
            Self::Error(_) => write!(f, "???"),
            Self::Atom(atom) => write!(f, "{}", atom),
            Self::Num(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::MakeClosure(closure) => write!(f, "{}", closure),
            Self::AppClosure(clo, args) => write!(f, "{}({})", clo, ", ".join(args)),
            Self::AppFunc(index, fun, args) => write!(f, "{}/{}({})", fun, index, ", ".join(args)),
            Self::AppExtern(ext, args) => write!(f, "{}({})", ext, ", ".join(args)),
            Self::BinOp(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            Self::If(cond, _, _) => write!(f, "if {} {{ .. }} else {{ .. }}", cond),
            Self::Record(fields, values) => write!(
                f,
                "{{{}}}",
                ", ".join(
                    fields
                        .iter()
                        .zip(values.iter())
                        .map(|(field, value)| lazy_format!("{} = {}", field, value))
                )
            ),
            Self::Project(record, index, field) => write!(f, "{}.{}/{}", record, field, index),
            Self::Variant(rank, constr, payload) => {
                write!(f, "{}/{}{}", constr, rank, in_parens_if_some(payload))
            }
            Self::Match(scrut, branches) => write!(
                f,
                "match {} {{ {}, }}",
                scrut,
                ", ".join(branches.iter().map(|branch| {
                    let Pattern { rank, constr, binder } = branch.pattern;
                    lazy_format!("{}/{}{} => ...", constr, rank, in_parens_if_some(&binder))
                }))
            ),
        }
    }
}

impl fmt::Display for IdxVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.1, self.0)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for MakeClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let MakeClosure { captured, params, body: _ } = self;
        write!(f, "[{}; {}; ...]", ", ".join(captured), ", ".join(params))
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Equals => "==",
            Self::NotEq => "!=",
            Self::Less => "<",
            Self::LessEq => "<=",
            Self::Greater => ">",
            Self::GreaterEq => ">=",
        })
    }
}
