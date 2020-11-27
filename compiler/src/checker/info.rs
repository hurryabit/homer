use super::types::RcType;
use crate::*;
use location::{Located, Span};
use syntax::ExprVar;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SymbolInfo<Loc> {
    ExprBinder { var: Located<ExprVar, Loc>, typ: RcType },
    ExprVar { var: Located<ExprVar, Loc>, typ: RcType, def: Span<Loc> },
}

impl<Loc> SymbolInfo<Loc> {
    pub fn span(&self) -> &Span<Loc> {
        match self {
            Self::ExprBinder { var, .. } | Self::ExprVar { var, .. } => &var.span,
        }
    }

    pub fn definition_span(&self) -> Option<&Span<Loc>> {
        match self {
            Self::ExprBinder { .. } => None,
            Self::ExprVar { def, .. } => Some(def),
        }
    }
}
