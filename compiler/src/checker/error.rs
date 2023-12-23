use super::types::*;
use super::Arity;
use crate::location::{Located, SourceSpan};
use crate::syntax;
use syntax::{ExprCon, ExprVar, TypeVar};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Undeclared type variable `{0}`.")]
    UnknownTypeVar(TypeVar),
    #[error(
        "Undeclared variable `{0}`.{}",
        if *.1 {
            format!(
                " There is a function of the same name.\n\
                If you want to use the function as a closure, you have to wrap it\n\
                explicitly: `fn (...) {{ {}(...) }}`.",
                .0
            )
        } else {
            String::new()
        }
    )]
    UnknownExprVar(ExprVar, bool), // bool indicateds if there's a function of the same name.
    #[error("Expected a type but found the generic type `{0}`.")]
    UnexpectedGeneric(TypeVar, Arity),
    // TODO(MH): Make error message better for expected == 0.
    #[error("{}", if *.expected == 0 {
            format!("Type `{type_var}` is not a generic type but is applied to {found} type argument(s).")
        } else {
            format!("Generic type `{type_var}` expects {expected} type argument(s) \
            but is applied to {found} type argument(s).")
        }

    )]
    GenericTypeArityMismatch { type_var: TypeVar, expected: Arity, found: Arity },
    #[error(
        "`{expr_var}` is a generic function that expects {expected} type argument(s) \
        but is applied to {found} type argument(s)."
    )]
    GenericFuncArityMismatch { expr_var: ExprVar, expected: Arity, found: Arity },
    #[error("`{expr_var}` is not a generic function and must be called as `{expr_var}(...)`.")]
    BadNonGenericCall { expr_var: ExprVar },
    #[error(
        "Expected an expression of type `{expected}` but found an expression of type `{found}`."
    )]
    TypeMismatch { expected: RcType, found: RcType },
    #[error("Expected parameter `{param}` to have type `{expected}` but found a type annotation `{found}`.")]
    ParamTypeMismatch { param: ExprVar, expected: RcType, found: RcType },
    #[error("Cannot infer the type of parameter `{0}`. A type annoation is needed.")]
    ParamNeedsType(ExprVar),
    #[error("Duplicate type variable `{var}`.")]
    DuplicateTypeVar { var: TypeVar, original: SourceSpan },
    #[error("Duplicate definition of type `{var}`.")]
    DuplicateTypeDecl { var: TypeVar, original: SourceSpan },
    #[error("Duplicate parameter `{var}`.")]
    DuplicateParam { var: ExprVar, original: SourceSpan },
    #[error("Duplicate definition of function `{var}`.")]
    DuplicateFuncDecl { var: ExprVar, original: SourceSpan },
    #[error(
        "Cannot apply {num_args} argument(s) to {} because it has has type `{func_type}`.",
        if let Some(func) = func {
            format!("`{}`", func)
        } else {
            String::from("expression")
        }
    )]
    BadApp { func: Option<ExprVar>, func_type: RcType, num_args: Arity },
    #[error("Expression of type `{record_type}` do not contain a field named `{field}`.")]
    BadRecordProj { record_type: RcType, field: ExprVar },
    #[error("Expected an expression of type `{0}` but found a lambda with {1} parameter(s).")]
    BadLam(RcType, Arity),
    #[error("Constructor `{constr}` of variant type `{variant_type}` needs a payload.")]
    VariantExpectedPayload { variant_type: RcType, constr: ExprCon },
    #[error("Constructor `{constr}` of variant type `{variant_type}` does not take a payload.")]
    VariantUnexpectedPayload { variant_type: RcType, constr: ExprCon },
    #[error("`{1}` is not a possible constructor for variant type `{0}`.")]
    BadVariantConstr(RcType, ExprCon),
    #[error("Expected an expression of type `{0}` but found variant constructor `{1}`.")]
    UnexpectedVariantType(RcType, ExprCon),
    #[error("Cannot match on expressions of type `{0}`.")]
    BadMatch(RcType),
    #[error("`{1}` is not a possible constructor for variant type `{0}`.")]
    BadBranch(RcType, ExprCon),
    #[error("Match expressions must have at least one branch.")]
    EmptyMatch,
    #[error("Constructor `{1}` is not covered in pattern match on type `{0}`.")]
    NonExhaustiveMatch(RcType, ExprCon),
    #[error("Constructor `{1}` is covered repeatedly in pattern match.")]
    OverlappingMatch(RcType, ExprCon),
    #[error("Cannot infer the type of the expression. Further type annotations are required.")]
    TypeAnnsNeeded,
}

pub type LError = Located<Error>;

impl LError {
    pub fn variant_payload<T, R>(
        opt_payload: &Option<T>,
        opt_payload_type: &Option<RcType>,
        variant_type: &RcType,
        constr: ExprCon,
        span: SourceSpan,
    ) -> Result<R, LError> {
        // TODO(MH): Use `Result<!, LError>` instead of `for<R> Result<R, LError>` as the result
        // type once the never type is stable.
        let variant_type = variant_type.clone();
        let error = match (opt_payload, opt_payload_type) {
            (None, None) | (Some(_), Some(_)) => {
                panic!("IMPOSSIBLE: Error::variant_payload with None/None or Some/Some");
            }
            (None, Some(_)) => Error::VariantExpectedPayload { variant_type, constr },
            (Some(_), None) => Error::VariantUnexpectedPayload { variant_type, constr },
        };
        Err(Located::new(error, span))
    }
}
