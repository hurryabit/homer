use super::types::RcType;
use crate::*;
use syntax::LExprVar;

type Span = location::Span<location::ParserLoc>;

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolInfo {
    ExprBinder { var: LExprVar, typ: RcType },
    ExprVar { var: LExprVar, typ: RcType, def: Span },
}

impl SymbolInfo {
    pub fn span(&self) -> Span {
        match self {
            Self::ExprBinder { var, .. } | Self::ExprVar { var, .. } => var.span,
        }
    }
}
