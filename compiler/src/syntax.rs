use crate::location;
use debug::DebugWriter;
use std::fmt;

mod debruijn;
mod debug;
#[macro_use]
mod ident;
mod iter;

type Located<T> = location::Located<T, location::ParserLoc>;

pub struct Module {
    pub decls: Vec<Decl>,
}

pub enum Decl {
    Type(TypeDecl),
    Func(FuncDecl),
}

pub struct TypeDecl {
    pub name: LTypeVar,
    pub params: Vec<LTypeVar>,
    pub body: LType,
}

pub struct FuncDecl {
    pub name: LExprVar,
    pub type_params: Vec<LTypeVar>,
    pub expr_params: Vec<(LExprVar, LType)>,
    pub return_type: LType,
    pub body: LExpr,
}

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

pub enum Expr {
    Error,
    Var(ExprVar),
    Num(i64),
    Bool(bool),
    Lam(Vec<(LExprVar, Option<LType>)>, Box<LExpr>),
    App(Box<LExpr>, Vec<LExpr>),
    BinOp(Box<LExpr>, OpCode, Box<LExpr>),
    FuncInst(LExprVar, Vec<LType>), // Instantiate function at monomorphic type.
    Let(LExprVar, Option<LType>, Box<LExpr>, Box<LExpr>),
    If(Box<LExpr>, Box<LExpr>, Box<LExpr>),
    Record(Vec<(LExprVar, LExpr)>),
    Proj(Box<LExpr>, LExprVar),
    Variant(ExprCon, Option<Box<LExpr>>),
    Match(Box<LExpr>, Vec<Branch>),
}

pub type LExpr = Located<Expr>;

pub struct Branch {
    pub pattern: LPattern,
    pub body: LExpr,
}

pub struct Pattern {
    pub constr: ExprCon,
    pub binder: Option<LExprVar>,
}

pub type LPattern = Located<Pattern>;

#[derive(Clone, Copy)]
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

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugWriter::fmt(self, f)
    }
}

impl fmt::Debug for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugWriter::fmt(self, f)
    }
}

impl fmt::Debug for FuncDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugWriter::fmt(self, f)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugWriter::fmt(self, f)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugWriter::fmt(self, f)
    }
}
