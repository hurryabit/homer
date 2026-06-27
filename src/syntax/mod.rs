mod debug;
mod ident;
mod iter;

use self::ident::ident_type;
pub use self::iter::ExprRef;
use crate::ast;
use crate::checker;
use crate::location::Located;

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

#[derive(Clone, Default, Eq, PartialEq)]
pub enum Type {
    #[default]
    Error,
    Var(LTypeVar),
    SynApp(LTypeVar, Vec<LType>),
    Int,
    Bool,
    Fun(Vec<LType>, Box<LType>),
    Record(Vec<(LExprVar, LType)>),
    Variant(Vec<(LExprCon, Option<LType>)>),
    Inferred(checker::RcType),
}

pub type LType = Located<Type>;

#[derive(Clone, Default, Eq, PartialEq)]
pub enum Expr {
    #[default]
    Error,
    Var(LExprVar),
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

ast::derive_fmt_debug!(Module);
ast::derive_fmt_debug!(Decl);
ast::derive_fmt_debug!(FuncDecl);
ast::derive_fmt_debug!(Type);
ast::derive_fmt_debug!(Expr);
