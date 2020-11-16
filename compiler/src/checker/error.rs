use super::types::*;
use super::Arity;
use crate::location::{Located, ParserLoc, Span};
use crate::syntax;
use std::fmt;
use syntax::{ExprCon, ExprVar, TypeVar};

#[derive(Debug)]
pub enum Error<Pos = ParserLoc> {
    UnknownTypeVar(TypeVar),
    UnknownExprVar(ExprVar),
    UnexpectedGeneric(TypeVar, Arity),
    GenericTypeArityMismatch {
        type_var: TypeVar,
        expected: Arity,
        found: Arity,
    },
    GenericFuncArityMismatch {
        expr_var: ExprVar,
        expected: Arity,
        found: Arity,
    },
    TypeMismatch {
        expected: RcType,
        found: RcType,
    },
    ParamTypeMismatch {
        param: ExprVar,
        expected: RcType,
        found: RcType,
    },
    ParamNeedsType(ExprVar),
    DuplicateTypeVar {
        var: TypeVar,
        original: Span<Pos>,
    },
    DuplicateTypeDecl {
        var: TypeVar,
        original: Span<Pos>,
    },
    DuplicateParam {
        var: ExprVar,
        original: Span<Pos>,
    },
    DuplicateFuncDecl {
        var: ExprVar,
        original: Span<Pos>,
    },
    BadApp {
        func: Option<ExprVar>,
        func_type: RcType,
        num_args: Arity,
    },
    BadRecordProj {
        record_type: RcType,
        field: ExprVar,
    },
    BadLam(RcType, Arity),
    VariantExpectedPayload {
        variant_type: RcType,
        constr: ExprCon,
    },
    VariantUnexpectedPayload {
        variant_type: RcType,
        constr: ExprCon,
    },
    BadVariantConstr(RcType, ExprCon),
    UnexpectedVariantType(RcType, ExprCon),
    BadMatch(RcType),
    BadBranch(RcType, ExprCon),
    EmptyMatch,
    TypeAnnsNeeded,
}

pub type LError<Pos = ParserLoc> = Located<Error<Pos>, Pos>;

impl LError {
    pub fn variant_payload<Pos, T, R>(
        opt_payload: &Option<T>,
        opt_payload_type: &Option<RcType>,
        variant_type: &RcType,
        constr: ExprCon,
        span: Span<Pos>,
    ) -> Result<R, LError<Pos>> {
        // TODO(MH): Use `!` instead of `()` once the never type is stable.
        let variant_type = variant_type.clone();
        let error = match (opt_payload, opt_payload_type) {
            (None, None) | (Some(_), Some(_)) => {
                panic!("IMPOSSIBLE: Error::variant_payload with None/None or Some/Some")
            }
            (None, Some(_)) => Error::VariantExpectedPayload {
                variant_type,
                constr,
            },
            (Some(_), None) => Error::VariantUnexpectedPayload {
                variant_type,
                constr,
            },
        };
        Err(Located::new(error, span))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn plural(n: Arity) -> &'static str {
            if n == 1 {
                ""
            } else {
                "s"
            }
        }

        use Error::*;
        match self {
            UnknownTypeVar(var) => write!(f, "Undeclared type variable `{}`.", var),
            UnknownExprVar(var) => write!(f, "Undeclared variable `{}`.", var),
            UnexpectedGeneric(var, _arity) => {
                write!(f, "Expected a type but found the generic type `{}`.", var)
            }
            GenericTypeArityMismatch {
                type_var,
                expected: 0,
                found,
            }  => write!(
                f,
                "Type `{}` is not a generic type but is applied to {} type argument{}.",
                type_var, found, plural(*found)
            ),
            GenericTypeArityMismatch {
                type_var,
                expected,
                found,
            } => write!(
                f,
                "Generic type `{}` expects {} type argument{} but is applied to {} type argument{}.",
                type_var, expected, plural(*expected), found, plural(*found)
            ),
            GenericFuncArityMismatch {
                expr_var,
                expected,
                found,
            } => {
                if *expected > 0 {
                    write!(
                    f,
                    "Generic function `{}` expects {} type argument{} but is applied to {} type argument{}.",
                    expr_var, expected, plural(*expected), found, plural(*found))
                } else if *found == 0 {
                    write!(f, "`{}` is not a generic function and must be called as `{}(...)`.", expr_var, expr_var)
                } else {
                    write!(
                        f,
                        "`{}` is not a generic function but is applied to {} type argument{}.",
                        expr_var, found, plural(*found)
                    )
                }
            }
            TypeMismatch { expected, found } => write!(
                f,
                "Expected an expression of type `{}` but found an expression of type `{}`.",
                expected, found,
            ),
            ParamTypeMismatch { param, expected, found } => write!(
                f,
                "Expected parameter `{}` to have type `{}` but found a type annotation `{}`.",
                param, expected, found,
            ),
            ParamNeedsType(param) => write!(f, "Cannot infer the type of parameter `{}`. A type annoation is needed.", param),
            DuplicateTypeVar { var, original: _ } => write!(f, "Duplicate type variable `{}`.", var),
            DuplicateTypeDecl { var, original: _ } => {
                write!(f, "Duplicate definition of type `{}`.", var)
            }
            DuplicateParam { var, original: _ } => write!(f, "Duplicate paramter `{}`.", var),
            DuplicateFuncDecl { var, original: _ } => {
                write!(f, "Duplicate definition of function `{}`.", var)
            }
            BadApp {
                func: Some(func),
                func_type,
                num_args,
            } => write!(
                f,
                "`{}` cannot be applied to {} argument{} because it has has type `{}`.",
                func, num_args, plural(*num_args), func_type
            ),
            BadApp {
                func: None,
                func_type,
                num_args,
            } => write!(
                f,
                "Expressions of type `{}` cannot be applied to {} argument{}.",
                func_type, num_args, plural(*num_args)
            ),
            BadRecordProj { record_type, field } => write!(
                f,
                "Expression of type `{}` do not contain a field named `{}`.",
                record_type, field
            ),
            BadLam(expected, arity) => write!(
                f,
                "Expected an expression of type `{}` but found a lambda with {} parameter{}.",
                expected, arity, plural(*arity)
            ),
            VariantExpectedPayload {
                variant_type, constr
            } => write!(f, "Constructor `{}` of variant type `{}` needs a payload.", constr, variant_type),
            VariantUnexpectedPayload {
                variant_type, constr
            } => write!(f, "Constructor `{}` of variant type `{}` does not take a payload.", constr, variant_type),
            BadVariantConstr(expected, con) => write!(
                f,
                "`{}` is not a possible constructor for variant type `{}`.",
                con, expected
            ),
            UnexpectedVariantType(expected, con) => write!(
                f,
                "Expected an expression of type `{}` but found variant constructor `{}`.",
                expected, con
            ),
            EmptyMatch => write!(f, "Match expressions must have at least one branch."),
            BadMatch(scrut_type) => {
                write!(f, "Cannot match on expressions of type `{}`.", scrut_type)
            }
            BadBranch(scrut_type, con) => write!(
                f,
                "`{}` is not a possible constructor for variant type `{}`.",
                con, scrut_type
            ),
            TypeAnnsNeeded => write!(f, "Cannot infer the type of the expression. Further type annotations are required."),
        }
    }
}
