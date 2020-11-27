use super::types::RcType;
use crate::*;
use location::{Located, SourceSpan};
use syntax::ExprVar;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SymbolInfo {
    ExprBinder { var: Located<ExprVar>, typ: RcType },
    ExprVar { var: Located<ExprVar>, typ: RcType, def: SourceSpan },
}

impl SymbolInfo {
    pub fn span(&self) -> &SourceSpan {
        match self {
            Self::ExprBinder { var, .. } | Self::ExprVar { var, .. } => &var.span,
        }
    }

    pub fn definition_span(&self) -> Option<&SourceSpan> {
        match self {
            Self::ExprBinder { .. } => None,
            Self::ExprVar { def, .. } => Some(def),
        }
    }
}
