use std::fmt;
use std::rc::Rc;

use crate::location;
use crate::syntax;
use syntax::{ExprCon, ExprVar, TypeVar};

type Located<T> = location::Located<T, location::ParserLoc>;

type SynType = syntax::Type;

#[derive(Debug)]
pub enum Type<T = RcType> {
    Error,
    Var(TypeVar),
    SynApp(TypeVar, Vec<T>),
    Int,
    Bool,
    Fun(Vec<T>, T),
    Record(Vec<(ExprVar, T)>),
    Variant(Vec<(ExprCon, Option<T>)>),
}

#[derive(Clone, Debug)]
pub struct RcType(Rc<Type>);

#[derive(Debug)]
pub struct TypeScheme {
    pub params: Vec<TypeVar>,
    pub body: RcType,
}

type TypeDefs = std::collections::HashMap<TypeVar, TypeScheme>;

impl RcType {
    pub fn new(typ: Type) -> Self {
        Self(Rc::new(typ))
    }

    pub fn from_lsyntax(lsyntax: &Located<SynType>) -> Self {
        Self::new(Type::from_syntax(&lsyntax.locatee))
    }

    pub fn from_syntax(syntax: &SynType) -> Self {
        Self::new(Type::from_syntax(syntax))
    }

    pub fn to_lsyntax(&self) -> Located<SynType> {
        Located::gen(self.to_syntax())
    }

    pub fn to_syntax(&self) -> SynType {
        Type::to_syntax(&*self)
    }

    pub fn subst(&self, mapping: &std::collections::HashMap<&TypeVar, &RcType>) -> RcType {
        match &**self {
            Type::Var(var) => {
                let &typ = mapping
                    .get(&var)
                    .expect("subst: free vars must be a subset of the substitution's domain");
                typ.clone()
            }
            typ => Self::new(typ.map(|child| child.subst(mapping))),
        }
    }

    pub fn weak_normalize(&self, type_defs: &TypeDefs) -> Self {
        let mut typ = self.clone();
        while let Type::SynApp(var, args) = &*typ {
            let scheme = type_defs.get(&var).unwrap();
            typ = scheme.instantiate(&args);
        }
        typ
    }

    pub fn equiv(&self, expected: &RcType, type_defs: &TypeDefs) -> bool {
        use Type::*;
        match (&**self, &**expected) {
            (SynApp(var1, args1), SynApp(var2, args2)) if var1 == var2 => {
                assert_eq!(args1.len(), args2.len());
                args1
                    .iter()
                    .zip(args2.iter())
                    .all(|(arg1, arg2)| arg1.equiv(arg2, type_defs))
            }
            _ => match (
                self.weak_normalize(type_defs).as_ref(),
                expected.weak_normalize(type_defs).as_ref(),
            ) {
                (SynApp(_, _), _) | (_, SynApp(_, _)) => {
                    panic!("IMPOSSIBLE: Type::SynApp after Type::weak_normalize")
                }
                (Error, _) | (_, Error) => true,
                (Var(var1), Var(var2)) => var1 == var2,
                (Int, Int) => true,
                (Bool, Bool) => true,
                (Fun(params1, result1), Fun(params2, result2)) => {
                    params1.len() == params2.len()
                        && params1
                            .iter()
                            .zip(params2.iter())
                            .all(|(param1, param2)| param1.equiv(param2, type_defs))
                        && result1.equiv(result2, type_defs)
                }
                (Record(fields1), Record(fields2)) => {
                    same_keys(&fields1, &fields2)
                        && fields1
                            .iter()
                            .zip(fields2.iter())
                            .all(|((_, typ1), (_, typ2))| typ1.equiv(typ2, type_defs))
                }
                (Variant(constrs1), Variant(constrs2)) => {
                    same_keys(&constrs1, &constrs2)
                        && constrs1.iter().zip(constrs2.iter()).all(
                            |((_, opt_typ1), (_, opt_typ2))| match (opt_typ1, opt_typ2) {
                                (None, None) => true,
                                (None, Some(_)) | (Some(_), None) => false,
                                (Some(typ1), Some(typ2)) => typ1.equiv(typ2, type_defs),
                            },
                        )
                }
                (Var(_), _)
                | (Int, _)
                | (Bool, _)
                | (Fun(_, _), _)
                | (Record(_), _)
                | (Variant(_), _) => false,
            },
        }
    }
}

impl TypeScheme {
    /// Instantiate a type scheme with the given types. Assumes that the
    /// number of parameters of the scheme and the number of given types match.
    pub fn instantiate(&self, types: &Vec<RcType>) -> RcType {
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
            SynType::Var(var) => Type::Var(*var),
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
        }
    }

    pub fn to_lsyntax(&self) -> Located<SynType> {
        Located::gen(self.to_syntax())
    }

    pub fn to_syntax(&self) -> SynType {
        match self {
            Type::Error => SynType::Error,
            Type::Var(var) => SynType::Var(*var),
            Type::SynApp(var, args) => {
                let args = args.iter().map(RcType::to_lsyntax).collect();
                SynType::SynApp(Located::gen(*var), args)
            }
            Type::Int => SynType::Int,
            Type::Bool => SynType::Bool,
            Type::Fun(params, result) => {
                let params = params.iter().map(RcType::to_lsyntax).collect();
                let result = Box::new(RcType::to_lsyntax(result));
                SynType::Fun(params, result)
            }
            Type::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, typ)| (Located::gen(*name), typ.to_lsyntax()))
                    .collect();
                SynType::Record(fields)
            }
            Type::Variant(constrs) => {
                let constrs = constrs
                    .iter()
                    .map(|(name, opt_typ)| {
                        (
                            Located::gen(*name),
                            opt_typ.as_ref().map(|typ| typ.to_lsyntax()),
                        )
                    })
                    .collect();
                SynType::Variant(constrs)
            }
        }
    }
}

impl<T> Type<T> {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut T> {
        use genawaiter::{rc::gen, yield_};
        use Type::*;
        gen!({
            match self {
                Error => {}
                Var(_) | Int | Bool => {}
                SynApp(syn, args) => {
                    let _: &TypeVar = syn; // We want this to break if change the type of `syn`.
                    for arg in args {
                        yield_!(arg);
                    }
                }
                Fun(params, result) => {
                    for param in params {
                        yield_!(param);
                    }
                    yield_!(result);
                }
                Record(fields) => {
                    for (_name, typ) in fields {
                        yield_!(typ);
                    }
                }
                Variant(constrs) => {
                    for (_name, opt_typ) in constrs {
                        if let Some(typ) = opt_typ {
                            yield_!(typ);
                        }
                    }
                }
            }
        })
        .into_iter()
    }

    pub fn map<U, F>(&self, f: F) -> Type<U>
    where
        F: Fn(&T) -> U + Copy,
    {
        use Type::*;
        match self {
            Error => Error,
            Var(var) => Var(*var),
            SynApp(var, args) => {
                let args = args.iter().map(f).collect();
                SynApp(*var, args)
            }
            Int => Int,
            Bool => Bool,
            Fun(params, result) => {
                let params = params.iter().map(&f).collect();
                let result = f(result);
                Fun(params, result)
            }
            Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, child)| (*name, f(child)))
                    .collect();
                Record(fields)
            }
            Variant(constrs) => {
                let constrs = constrs
                    .iter()
                    .map(|(name, opt_child)| (*name, opt_child.as_ref().map(f)))
                    .collect();
                Variant(constrs)
            }
        }
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
    vec1.len() == vec2.len()
        && vec1
            .iter()
            .zip(vec2.iter())
            .all(|((k1, _), (k2, _))| k1 == k2)
}

impl fmt::Display for RcType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // NOTE(MH): Let's make sure we call the right method and don't end up
        // in an inifite loop.
        let typ: &Type = &*self;
        typ.fmt(f)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn write_list<T, F>(fmt: &mut fmt::Formatter, list: &[T], sep: &str, f: F) -> fmt::Result
        where
            F: Fn(&mut fmt::Formatter, &T) -> fmt::Result,
        {
            let mut first = true;
            for item in list {
                if first {
                    first = false;
                } else {
                    fmt.write_str(sep)?;
                }
                f(fmt, item)?;
            }
            Ok(())
        }

        use Type::*;
        match self {
            Type::Error => write!(f, "???"),
            Var(var) => write!(f, "{}", var),
            SynApp(syn, args) => {
                write!(f, "{}", syn)?;
                if args.len() > 0 {
                    write!(f, "<")?;
                    write_list(f, &args, ", ", |f, arg| write!(f, "{}", arg))?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            Int => write!(f, "Int"),
            Bool => write!(f, "Bool"),
            Fun(params, result) => {
                write!(f, "(")?;
                write_list(f, &params, ", ", |f, param| write!(f, "{}", param))?;
                write!(f, ") -> {}", result)
            }
            Record(fields) => {
                f.write_str("{")?;
                write_list(f, &fields, ", ", |f, (field, typ)| {
                    write!(f, "{}: {}", field, typ)
                })?;
                f.write_str("}")
            }
            Variant(constrs) => {
                f.write_str("[")?;
                write_list(f, &constrs, " | ", |f, (constr, opt_typ)| {
                    if let Some(typ) = opt_typ {
                        write!(f, "{}({})", constr, typ)
                    } else {
                        write!(f, "{}", constr)
                    }
                })?;
                f.write_str("]")
            }
        }
    }
}
