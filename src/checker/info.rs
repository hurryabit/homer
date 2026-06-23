use super::types::{FuncSig, RcType};
use crate::*;
use location::SourceSpan;
use std::sync::Arc;
use syntax::LExprVar;

#[derive(Clone, Eq, PartialEq)]
pub enum SymbolInfo {
    ExprBinder { var: LExprVar, typ: RcType },
    ExprVar { var: LExprVar, typ: RcType, def: SourceSpan },
    FuncRef { var: LExprVar, def: Arc<FuncSig> },
}

impl SymbolInfo {
    pub fn span(&self) -> &SourceSpan {
        match self {
            Self::ExprBinder { var, .. }
            | Self::ExprVar { var, .. }
            | Self::FuncRef { var, .. } => &var.span,
        }
    }

    pub fn definition_span(&self) -> Option<&SourceSpan> {
        match self {
            Self::ExprBinder { .. } => None,
            Self::ExprVar { def, .. } => Some(def),
            Self::FuncRef { def, .. } => Some(&def.name.span),
        }
    }
}

impl ast::Debug for SymbolInfo {
    fn write(&self, writer: &mut ast::DebugWriter) -> std::fmt::Result {
        match self {
            Self::ExprBinder { var, typ } => writer.node("EXPR_BINDER", |writer| {
                writer.child("var", var)?;
                writer.child("type", typ)
            }),
            Self::ExprVar { var, typ, def } => writer.node("EXPR_VAR", |writer| {
                writer.child("var", var)?;
                writer.child("type", typ)?;
                writer.child("def", def)
            }),
            Self::FuncRef { var, def } => writer.node("FUNC_REF", |writer| {
                writer.child("var", var)?;
                writer.child("def", def)
            }),
        }
    }
}

derive_fmt_debug!(SymbolInfo);
