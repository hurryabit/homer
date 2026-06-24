use join_lazy_fmt::*;
use std::fmt;
use std::sync::{Arc, OnceLock};

use crate::location::Located;
use crate::util::in_parens_if_some;
use crate::{ast, syntax};
use syntax::{ExprCon, ExprVar, LExprVar, TypeVar};

pub use syntax::Type as SynType;

#[derive(Eq, PartialEq, Hash)]
pub enum Type<T = RcType> {
    Error,
    Var(TypeVar),
    SynApp(TypeVar, Vec<T>),
    Int,
    Bool,
    Fun(Vec<T>, T),
    Record(Vec<(ExprVar, T)>),
    Variant(Vec<(ExprCon, Option<T>)>),
    UnificationVar(UnificationVar),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct RcType(Arc<Type>);

pub struct UnificationVar {
    id: u32,
    link: OnceLock<RcType>,
}

#[derive(Eq, PartialEq)]
pub struct FuncSig {
    pub name: LExprVar,
    pub type_params: Vec<TypeVar>,
    pub params: Vec<(ExprVar, RcType)>,
    pub result: RcType,
}

pub struct TypeScheme {
    pub params: Vec<TypeVar>,
    pub body: RcType,
}

type TypeDefs = std::collections::HashMap<TypeVar, TypeScheme>;

impl RcType {
    pub fn new(typ: Type) -> Self {
        Self(Arc::new(typ))
    }

    pub fn as_inferred(&self, var: &LExprVar) -> Located<SynType> {
        Located::new(SynType::Inferred(self.clone()), var.span)
    }

    pub fn from_lsyntax(lsyntax: &Located<SynType>) -> Self {
        Self::new(Type::from_syntax(&lsyntax.locatee))
    }

    pub fn from_syntax(syntax: &SynType) -> Self {
        Self::new(Type::from_syntax(syntax))
    }

    pub fn subst(&self, mapping: &std::collections::HashMap<&TypeVar, &RcType>) -> RcType {
        match &**self {
            Type::Var(var) => {
                let &typ = mapping
                    .get(&var)
                    .expect("subst: free vars must be a subset of the substitution's domain");
                typ.clone()
            }
            Type::Error | Type::Int | Type::Bool => self.clone(),
            Type::SynApp(var, args) => {
                let args = args.iter().map(|child| child.subst(mapping)).collect();
                Self::new(Type::SynApp(*var, args))
            }
            Type::Fun(params, result) => {
                let params = params.iter().map(|child| child.subst(mapping)).collect();
                let result = result.subst(mapping);
                Self::new(Type::Fun(params, result))
            }
            Type::Record(fields) => {
                let fields =
                    fields.iter().map(|(name, child)| (*name, child.subst(mapping))).collect();
                Self::new(Type::Record(fields))
            }
            Type::Variant(constrs) => {
                let constrs = constrs
                    .iter()
                    .map(|(name, opt_child)| {
                        (*name, opt_child.as_ref().map(|child| child.subst(mapping)))
                    })
                    .collect();
                Self::new(Type::Variant(constrs))
            }
            Type::UnificationVar(_) => panic!("subst: unification var in substitution target"),
        }
    }

    pub fn weak_normalize(&self, type_defs: &TypeDefs) -> Self {
        let mut typ = self.clone();
        loop {
            match typ.as_ref() {
                Type::SynApp(syn, args) => {
                    let scheme = type_defs.get(syn).unwrap();
                    typ = scheme.instantiate(args);
                }
                Type::UnificationVar(uvar) => {
                    let Some(link) = uvar.link.get() else {
                        break;
                    };
                    typ = link.clone();
                }
                _ => break,
            }
        }
        typ
    }

    pub fn equiv(&self, expected: &RcType, type_defs: &TypeDefs) -> bool {
        match (self.as_ref(), expected.as_ref()) {
            (Type::SynApp(var1, args1), Type::SynApp(var2, args2)) if var1 == var2 => {
                assert_eq!(args1.len(), args2.len());
                args1.iter().zip(args2.iter()).all(|(arg1, arg2)| arg1.equiv(arg2, type_defs))
            }
            _ => match (
                self.weak_normalize(type_defs).as_ref(),
                expected.weak_normalize(type_defs).as_ref(),
            ) {
                (Type::SynApp(_, _), _) | (_, Type::SynApp(_, _)) => {
                    panic!("IMPOSSIBLE: Type::SynApp after Type::weak_normalize")
                }
                (Type::UnificationVar(uvar1), Type::UnificationVar(uvar2)) => {
                    assert!(uvar1.is_free() && uvar2.is_free());
                    if uvar1.id != uvar2.id {
                        uvar1.set_link(expected.clone())
                    }
                    true
                }
                (Type::UnificationVar(uvar1), _) => uvar1.try_set_link(expected.clone()),
                (_, Type::UnificationVar(uvar2)) => uvar2.try_set_link(self.clone()),
                (Type::Error, _) | (_, Type::Error) => true,
                (Type::Var(var1), Type::Var(var2)) => var1 == var2,
                (Type::Int, Type::Int) => true,
                (Type::Bool, Type::Bool) => true,
                (Type::Fun(params1, result1), Type::Fun(params2, result2)) => {
                    params1.len() == params2.len()
                        && params1
                            .iter()
                            .zip(params2.iter())
                            .all(|(param1, param2)| param1.equiv(param2, type_defs))
                        && result1.equiv(result2, type_defs)
                }
                (Type::Record(fields1), Type::Record(fields2)) => {
                    same_keys(fields1, fields2)
                        && fields1
                            .iter()
                            .zip(fields2.iter())
                            .all(|((_, typ1), (_, typ2))| typ1.equiv(typ2, type_defs))
                }
                (Type::Variant(constrs1), Type::Variant(constrs2)) => {
                    same_keys(constrs1, constrs2)
                        && constrs1.iter().zip(constrs2.iter()).all(
                            |((_, opt_typ1), (_, opt_typ2))| match (opt_typ1, opt_typ2) {
                                (None, None) => true,
                                (None, Some(_)) | (Some(_), None) => false,
                                (Some(typ1), Some(typ2)) => typ1.equiv(typ2, type_defs),
                            },
                        )
                }
                (Type::Var(_), _)
                | (Type::Int, _)
                | (Type::Bool, _)
                | (Type::Fun(_, _), _)
                | (Type::Record(_), _)
                | (Type::Variant(_), _) => false,
            },
        }
    }

    fn occurs_check(&self, id: u32) -> bool {
        match self.as_ref() {
            Type::UnificationVar(uvar) => match uvar.link() {
                None => uvar.id != id,
                Some(typ) => typ.occurs_check(id),
            },
            typ => typ.children().all(|child| child.occurs_check(id)),
        }
    }
}

impl FuncSig {
    /// Instantiate a generic function signature with the given types. Assumes
    /// the number of given types matches the number of type paramters of the
    /// signature.
    pub fn instantiate(&self, types: &[RcType]) -> (Vec<RcType>, RcType) {
        let Self { name: _, type_params, params, result } = self;
        assert_eq!(type_params.len(), types.len());
        let mapping = type_params.iter().zip(types.iter()).collect();
        let params = params.iter().map(|(_var, typ)| typ.subst(&mapping)).collect();
        let result = result.subst(&mapping);
        (params, result)
    }
}

impl TypeScheme {
    /// Instantiate a type scheme with the given types. Assumes that the
    /// number of parameters of the scheme and the number of given types match.
    pub fn instantiate(&self, types: &[RcType]) -> RcType {
        let Self { params, body } = self;
        assert_eq!(params.len(), types.len());
        let mapping = params.iter().zip(types.iter()).collect();
        body.subst(&mapping)
    }
}

impl Type {
    pub fn from_syntax(syntax: &SynType) -> Self {
        match syntax {
            SynType::Error => Type::Error,
            SynType::Var(var) => Type::Var(var.locatee),
            SynType::SynApp(var, args) => {
                let args = args.iter().map(RcType::from_lsyntax).collect();
                Type::SynApp(var.locatee, args)
            }
            SynType::Int => Type::Int,
            SynType::Bool => Type::Bool,
            SynType::Fun(params, result) => {
                let params = params.iter().map(RcType::from_lsyntax).collect();
                let result = RcType::from_lsyntax(result);
                Type::Fun(params, result)
            }
            SynType::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, typ)| (name.locatee, RcType::from_lsyntax(typ)))
                    .collect();
                Type::Record(fields)
            }
            SynType::Variant(constrs) => {
                let constrs = constrs
                    .iter()
                    .map(|(name, opt_typ)| {
                        (name.locatee, opt_typ.as_ref().map(RcType::from_lsyntax))
                    })
                    .collect();
                Type::Variant(constrs)
            }
            SynType::Inferred(_) => panic!("IMPOSSIBLE: only introduced by type checker"),
        }
    }
}

impl<T> Type<T> {
    pub fn children(&self) -> impl Iterator<Item = &T> {
        use genawaiter::{rc::r#gen, yield_};
        r#gen!({
            match self {
                Self::Error => {}
                Self::Var(_) | Self::Int | Self::Bool => {}
                Self::SynApp(syn, args) => {
                    let _: &TypeVar = syn; // We want this to break if change the type of `syn`.
                    for arg in args {
                        yield_!(arg);
                    }
                }
                Self::Fun(params, result) => {
                    for param in params {
                        yield_!(param);
                    }
                    yield_!(result);
                }
                Self::Record(fields) => {
                    for (_name, typ) in fields {
                        yield_!(typ);
                    }
                }
                Self::Variant(constrs) => {
                    for (_name, opt_typ) in constrs {
                        if let Some(typ) = opt_typ {
                            yield_!(typ);
                        }
                    }
                }
                Self::UnificationVar(_) => {}
            }
        })
        .into_iter()
    }
}

impl std::ops::Deref for RcType {
    type Target = Type;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl AsRef<Type> for RcType {
    fn as_ref(&self) -> &Type {
        self.0.as_ref()
    }
}

pub fn same_keys<'a, K: Eq, V>(vec1: &'a [(K, V)], vec2: &'a [(K, V)]) -> bool {
    vec1.len() == vec2.len() && vec1.iter().zip(vec2.iter()).all(|((k1, _), (k2, _))| k1 == k2)
}

impl fmt::Display for RcType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Error => write!(f, "???"),
            Self::Var(var) => write!(f, "{}", var),
            Self::SynApp(syn, args) => {
                write!(f, "{}", syn)?;
                if !args.is_empty() {
                    write!(f, "<{}>", ", ".join(args))?;
                }
                Ok(())
            }
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Fun(params, result) => write!(f, "({}) -> {}", ", ".join(params), result),
            Self::Record(fields) => write!(
                f,
                "{{{}}}",
                ", ".join(fields.iter().map(|(field, typ)| lazy_format!("{}: {}", field, typ)))
            ),
            Self::Variant(constrs) => {
                write!(
                    f,
                    "[{}]",
                    " | ".join(constrs.iter().map(|(constr, typ)| {
                        lazy_format!("{}{}", constr, in_parens_if_some(typ))
                    }))
                )
            }
            Self::UnificationVar(uvar) => write!(f, "{}", uvar),
        }
    }
}

impl fmt::Display for UnificationVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.link() {
            None => write!(f, "?{}", self.id),
            Some(typ) => typ.fmt(f),
        }
    }
}

impl fmt::Display for FuncSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name, type_params, params, result } = self;
        write!(f, "fn {}", name.locatee)?;
        if !type_params.is_empty() {
            write!(f, "<{}>", ", ".join(type_params))?;
        }
        write!(
            f,
            "({}) -> {}",
            ", ".join(params.iter().map(|(var, typ)| lazy_format!("{}: {}", var, typ))),
            result
        )
    }
}

impl ast::Debug for Type {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self {
            Self::Error => writer.leaf("ERROR"),
            Self::Var(var) => var.write(writer),
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
            Self::UnificationVar(uvar) => uvar.write(writer),
        }
    }
}

impl ast::Debug for RcType {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        self.as_ref().write(writer)
    }
}

impl ast::Debug for UnificationVar {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        match self.link() {
            None => writer.node("FREE", |writer| writer.child("name", &self.id)),
            Some(typ) => writer.node("LINK", |writer| writer.child("typ", &typ)),
        }
    }
}

impl ast::Debug for TypeScheme {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { params, body } = self;
        writer.node("TYPE_SCHEME", |writer| {
            writer.children("param", params)?;
            writer.child("body", body)
        })
    }
}

impl ast::Debug for FuncSig {
    fn write(&self, writer: &mut ast::DebugWriter) -> fmt::Result {
        let Self { name, type_params, params, result } = self;
        writer.node("FUNC_SIG", |writer| {
            writer.child("name", name)?;
            writer.children("type_param", type_params)?;
            writer.children_pair("param_name", "param_type", params)?;
            writer.child("result", result)
        })
    }
}

derive_fmt_debug!(Type);
derive_fmt_debug!(RcType);
derive_fmt_debug!(TypeScheme);
derive_fmt_debug!(FuncSig);

impl UnificationVar {
    pub fn new_free(id: u32) -> Self {
        Self { id, link: OnceLock::new() }
    }

    fn is_free(&self) -> bool {
        self.link.get().is_none()
    }

    fn link(&self) -> Option<&RcType> {
        self.link.get()
    }

    fn set_link(&self, typ: RcType) {
        self.link.set(typ).unwrap();
    }

    fn try_set_link(&self, typ: RcType) -> bool {
        assert!(self.is_free());
        let result = typ.occurs_check(self.id);
        if result {
            self.link.set(typ).unwrap();
        } else {
            eprintln!("Occurs check failed on {}", typ);
        }
        result
    }
}

impl PartialEq for UnificationVar {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for UnificationVar {}

impl std::hash::Hash for UnificationVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
