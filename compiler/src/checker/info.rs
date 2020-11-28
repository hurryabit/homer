use super::types::{FuncSig, RcType};
use crate::*;
use location::SourceSpan;
use std::rc::Rc;
use syntax::LExprVar;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SymbolInfo {
    ExprBinder { var: LExprVar, typ: RcType },
    ExprVar { var: LExprVar, typ: RcType, def: SourceSpan },
    FuncRef { var: LExprVar, def: Rc<FuncSig> },
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
