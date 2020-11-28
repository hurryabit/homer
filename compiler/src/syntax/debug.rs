use super::*;
use crate::ast;
use std::fmt;

impl ast::Debug for Module {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { decls } = self;
        writer.node("MODULE", |writer| writer.children("decl", decls))
    }
}

impl ast::Debug for Decl {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Type(decl) => decl.write(writer),
            Self::Func(decl) => decl.write(writer),
        }
    }
}

impl ast::Debug for TypeDecl {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { name, params, body } = self;
        writer.node("TYPEDECL", |writer| {
            writer.child("name", name)?;
            writer.children("type_param", params)?;
            writer.child("body", body)
        })
    }
}

impl ast::Debug for FuncDecl {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { name, type_params, expr_params, return_type, body } = self;
        writer.node("FUNCDECL", |writer| {
            writer.child("name", name)?;
            writer.children("type_param", type_params)?;
            writer.children_pair("param", "type", expr_params)?;
            writer.child("result", return_type)?;
            writer.child("body", body)
        })
    }
}

impl ast::Debug for TypeVar {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
    }
}

impl ast::Debug for Type {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Error => writer.leaf("ERROR"),
            Self::Var(name) => name.write(writer),
            Self::SynApp(syn, args) => writer.node("APP", |writer| {
                writer.child("syn", syn)?;
                writer.children("type_arg", args)
            }),
            Self::Int => writer.leaf("INT"),
            Self::Bool => writer.leaf("BOOL"),
            Self::Fun(params, result) => writer.node("FUN", |writer| {
                writer.children("param", params)?;
                writer.child("result", result)
            }),
            Self::Record(fields) => {
                writer.node("RECORD", |writer| writer.children_pair("field", "type", fields))
            }
            Self::Variant(constrs) => writer.node("VARIANT", |writer| {
                for (constr, opt_typ) in constrs {
                    writer.child("constr", constr)?;
                    writer.child_if_some("type", opt_typ)?;
                }
                Ok(())
            }),
            Self::Inferred(typ) => writer.node("INFERRED", |writer| writer.child("type", typ)),
        }
    }
}

impl ast::Debug for ExprVar {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
    }
}

impl ast::Debug for (ExprVar, u32) {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        writer.leaf(&format!("{}/{}", self.0.as_str(), self.1))
    }
}

impl ast::Debug for (ExprVar, Option<u32>) {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self.1 {
            None => self.0.write(writer),
            Some(n) => (self.0, n).write(writer),
        }
    }
}

impl ast::Debug for ExprCon {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
    }
}

impl ast::Debug for (ExprCon, u32) {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        writer.leaf(&format!("{}/{}", self.0.as_str(), self.1))
    }
}

impl ast::Debug for (ExprCon, Option<u32>) {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self.1 {
            None => self.0.write(writer),
            Some(n) => (self.0, n).write(writer),
        }
    }
}

impl ast::Debug for Expr {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Error => writer.leaf("ERROR"),
            Self::Var(name) => name.write(writer),
            Self::Num(n) => writer.leaf(&n.to_string()),
            Self::Bool(b) => writer.leaf(&b.to_string()),
            Self::Lam(params, body) => writer.node("LAM", |writer| {
                for (param, opt_typ) in params {
                    writer.child("param", param)?;
                    writer.child_if_some("type", opt_typ)?;
                }
                writer.child("body", body)
            }),
            Self::AppClo(fun, args) => writer.node("APPCLO", |writer| {
                writer.child("clo", fun)?;
                writer.children("arg", args)
            }),
            Self::AppFun(fun, types, args) => writer.node("APPFUN", |writer| {
                writer.child("fun", fun)?;
                writer.children("type_arg", types.as_ref().unwrap_or(&vec![]))?;
                writer.children("arg", args)
            }),
            Self::BinOp(lhs, op, rhs) => writer.node("BINOP", |writer| {
                writer.child("lhs", lhs)?;
                writer.child("op", op)?;
                writer.child("rhs", rhs)
            }),
            Self::Let(binder, opt_typ, bindee, tail) => writer.node("LET", |writer| {
                writer.child("binder", binder)?;
                writer.child_if_some("type", opt_typ)?;
                writer.child("bindee", bindee)?;
                writer.child("tail", tail)
            }),
            Self::If(cond, then, elze) => writer.node("IF", |writer| {
                writer.child("cond", cond)?;
                writer.child("then", then)?;
                writer.child("else", elze)
            }),
            Self::Record(fields) => {
                writer.node("RECORD", |writer| writer.children_pair("field", "value", fields))
            }
            Self::Proj(record, field, index) => writer.node("PROJ", |writer| {
                writer.child("record", record)?;
                writer.child("field", &field.map(|f| (f, *index)))
            }),
            Self::Variant(constr, rank, payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", &(*constr, *rank))?;
                writer.child_if_some("payload", payload)?;
                Ok(())
            }),
            Self::Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                writer.children("branch", branches)
            }),
        }
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
        let Self { constr, rank, binder } = self;
        writer.node("PATTERN", |writer| {
            writer.child("constr", &(*constr, *rank))?;
            if let Some(binder) = binder {
                writer.child("binder", binder)?;
            }
            Ok(())
        })
    }
}

impl ast::Debug for OpCode {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Add => writer.leaf("ADD"),
            Self::Sub => writer.leaf("SUB"),
            Self::Mul => writer.leaf("MUL"),
            Self::Div => writer.leaf("DIV"),
            Self::Equals => writer.leaf("EQUALS"),
            Self::NotEq => writer.leaf("NOTEQ"),
            Self::Less => writer.leaf("LESS"),
            Self::LessEq => writer.leaf("LESSEQ"),
            Self::Greater => writer.leaf("GREATER"),
            Self::GreaterEq => writer.leaf("GREATEREQ"),
        }
    }
}
