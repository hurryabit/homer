use crate::location;
use debug::DebugWriter;
use std::fmt;

#[macro_use]
pub mod debug;
#[macro_use]
mod ident;
mod iter;

type Located<T> = location::Located<T, location::ParserLoc>;

#[derive(Clone, Eq, PartialEq)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Decl {
    Type(TypeDecl),
    Func(FuncDecl),
}

#[derive(Clone, Eq, PartialEq)]
pub struct TypeDecl {
    pub name: LTypeVar,
    pub params: Vec<LTypeVar>,
    pub body: LType,
}

#[derive(Clone, Eq, PartialEq)]
pub struct FuncDecl {
    pub name: LExprVar,
    pub type_params: Vec<LTypeVar>,
    pub expr_params: Vec<(LExprVar, LType)>,
    pub return_type: LType,
    pub body: LExpr,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Error,
    Var(TypeVar),
    SynApp(LTypeVar, Vec<LType>),
    Int,
    Bool,
    Fun(Vec<LType>, Box<LType>),
    Record(Vec<(LExprVar, LType)>),
    Variant(Vec<(LExprCon, Option<LType>)>),
}

pub type LType = Located<Type>;

#[derive(Clone, Eq, PartialEq)]
pub enum Expr {
    Error,
    Var(ExprVar),
    Num(i64),
    Bool(bool),
    Lam(Vec<(LExprVar, Option<LType>)>, Box<LExpr>),
    AppClo(LExprVar, Vec<LExpr>),
    AppFun(LExprVar, Option<Vec<LType>>, Vec<LExpr>),
    BinOp(Box<LExpr>, OpCode, Box<LExpr>),
    Let(LExprVar, Option<LType>, Box<LExpr>, Box<LExpr>),
    If(Box<LExpr>, Box<LExpr>, Box<LExpr>),
    Record(Vec<(LExprVar, LExpr)>),
    Proj(Box<LExpr>, LExprVar, Option<u32>),
    Variant(ExprCon, Option<u32>, Option<Box<LExpr>>),
    Match(Box<LExpr>, Vec<Branch>),
}

pub type LExpr = Located<Expr>;

#[derive(Clone, Eq, PartialEq)]
pub struct Branch {
    pub pattern: LPattern,
    pub rhs: LExpr,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Pattern {
    pub constr: ExprCon,
    pub rank: Option<u32>,
    pub binder: Option<LExprVar>,
}

pub type LPattern = Located<Pattern>;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

ident_type!(TypeVar);
pub type LTypeVar = Located<TypeVar>;

ident_type!(ExprVar);
pub type LExprVar = Located<ExprVar>;

ident_type!(ExprCon);
pub type LExprCon = Located<ExprCon>;

impl Default for Type {
    fn default() -> Self {
        Self::Error
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self::Error
    }
}

derive_fmt_debug!(Module);
derive_fmt_debug!(Decl);
derive_fmt_debug!(FuncDecl);
derive_fmt_debug!(Type);
derive_fmt_debug!(Expr);
