use super::types::RcType;
use crate::*;
use location::{HumanLoc, Humanizer, Located, ParserLoc, Span};
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

impl SymbolInfo<ParserLoc> {
    pub fn humanize(&self, humanizer: &Humanizer) -> SymbolInfo<HumanLoc> {
        match self {
            Self::ExprBinder { var, typ } => {
                SymbolInfo::ExprBinder { var: var.humanize(humanizer), typ: typ.clone() }
            }
            Self::ExprVar { var, typ, def } => SymbolInfo::ExprVar {
                var: var.humanize(humanizer),
                typ: typ.clone(),
                def: def.humanize(humanizer),
            },
        }
    }
}
