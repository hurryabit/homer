use crate::*;
use diagnostic::*;
use error::{Error, LError};
use location::Humanizer;
use std::collections;
use std::hash::Hash;
use std::rc::Rc;
use syntax::*;
use types::*;

mod error;
mod types;

type Span = location::Span<location::ParserLoc>;

type Located<T> = location::Located<T, location::ParserLoc>;

type Arity = usize;

type Type = types::Type;

#[derive(Clone)]
struct Env {
    builtin_types: Rc<collections::HashMap<TypeVar, Box<dyn Fn() -> syntax::Type>>>,
    type_defs: Rc<collections::HashMap<TypeVar, TypeScheme>>,
    func_sigs: Rc<collections::HashMap<ExprVar, TypeScheme>>,
    type_vars: im::HashSet<TypeVar>,
    expr_vars: im::HashMap<ExprVar, RcType>,
}

impl Module {
    pub fn check(&mut self, humanizer: &Humanizer) -> Result<(), Diagnostic> {
        self.check_impl().map_err(|error| Diagnostic {
            span: error.span.humanize(humanizer),
            severity: Severity::Error,
            source: Source::Checker,
            message: format!("{}", error.locatee),
        })
    }

    fn check_impl(&mut self) -> Result<(), LError> {
        if let Some((span, name)) = find_duplicate(self.type_decls().map(|decl| decl.name.as_ref()))
        {
            return Err(Located::new(
                Error::DuplicateTypeDecl {
                    var: *name.locatee,
                    original: span,
                },
                name.span,
            ));
        }
        if let Some((span, name)) = find_duplicate(self.func_decls().map(|decl| decl.name.as_ref()))
        {
            return Err(Located::new(
                Error::DuplicateFuncDecl {
                    var: *name.locatee,
                    original: span,
                },
                name.span,
            ));
        }
        let mut env = Env::new();
        env.type_defs = Rc::new(self.type_defs());
        for type_decl in self.type_decls_mut() {
            type_decl.check(&env)?;
        }
        env.type_defs = Rc::new(self.type_defs());
        let func_sigs = self
            .func_decls_mut()
            .map(|decl| Ok((decl.name.locatee, decl.check_signature(&env)?)))
            .collect::<Result<_, _>>()?;
        env.func_sigs = Rc::new(func_sigs);
        for decl in self.func_decls_mut() {
            decl.check(&env)?;
        }
        Ok(())
    }

    fn type_defs(&self) -> collections::HashMap<TypeVar, TypeScheme> {
        self.type_decls()
            .map(|TypeDecl { name, params, body }| {
                (
                    name.locatee,
                    TypeScheme {
                        params: params.iter().map(|param| param.locatee).collect(),
                        body: RcType::from_lsyntax(body),
                    },
                )
            })
            .collect()
    }
}

impl TypeDecl {
    fn check(&mut self, env: &Env) -> Result<(), LError> {
        let Self {
            name: _,
            params,
            body,
        } = self;
        LTypeVar::check_unique(params.iter())?;
        let env = &mut env.clone();
        env.type_vars = params.iter().map(|param| param.locatee).collect();
        body.check(env)
    }
}

impl FuncDecl {
    fn check_signature(&mut self, env: &Env) -> Result<TypeScheme, LError> {
        let Self {
            name: _,
            type_params,
            expr_params,
            return_type,
            body: _,
        } = self;
        LTypeVar::check_unique(type_params.iter())?;
        let env = &mut env.clone();
        env.type_vars = type_params.iter().map(|param| param.locatee).collect();
        for (_, typ) in expr_params.iter_mut() {
            typ.check(env)?;
        }
        return_type.check(env)?;
        Ok(TypeScheme {
            params: type_params.iter().map(|param| param.locatee).collect(),
            body: RcType::new(Type::Fun(
                expr_params
                    .iter()
                    .map(|(_, typ)| RcType::from_lsyntax(typ))
                    .collect(),
                RcType::from_lsyntax(return_type),
            )),
        })
    }

    fn check(&mut self, env: &Env) -> Result<(), LError> {
        let Self {
            name: _,
            type_params,
            expr_params,
            return_type,
            body,
        } = self;
        let env = &mut env.clone();
        env.type_vars = type_params.iter().map(|param| param.locatee).collect();
        env.intro_params(
            expr_params
                .iter()
                .map(|(var, typ)| Ok((*var, RcType::from_lsyntax(typ)))),
            |env| body.check(env, &RcType::from_lsyntax(return_type)),
        )
    }
}

impl LType {
    fn check(&mut self, env: &Env) -> Result<(), LError> {
        self.locatee.check(self.span, env)
    }
}

impl syntax::Type {
    fn check(&mut self, span: Span, env: &Env) -> Result<(), LError> {
        match self {
            Self::Error => Ok(()),
            Self::Int => panic!("Int in Type.check"),
            Self::Bool => panic!("Bool in Type.check"),
            Self::Var(var) => {
                if env.type_vars.contains(var) {
                    Ok(())
                } else if let Some(scheme) = env.type_defs.get(var) {
                    let arity = scheme.params.len();
                    if arity == 0 {
                        *self = Self::SynApp(Located::new(*var, span), vec![]);
                        Ok(())
                    } else {
                        Err(Located::new(Error::UnexpectedGeneric(*var, arity), span))
                    }
                } else if let Some(builtin) = env.builtin_types.get(var) {
                    *self = builtin();
                    Ok(())
                } else {
                    Err(Located::new(Error::UnknownTypeVar(*var), span))
                }
            }
            Self::SynApp(var, args) => {
                let num_args = args.len();
                assert!(num_args > 0);
                if env.type_vars.contains(&var.locatee) {
                    Err(Located::new(
                        Error::GenericTypeArityMismatch {
                            type_var: var.locatee,
                            expected: 0,
                            found: num_args,
                        },
                        span,
                    ))
                } else if let Some(scheme) = env.type_defs.get(&var.locatee) {
                    let arity = scheme.params.len();
                    if arity == num_args {
                        for arg in args {
                            arg.check(env)?;
                        }
                        Ok(())
                    } else {
                        Err(Located::new(
                            Error::GenericTypeArityMismatch {
                                type_var: var.locatee,
                                expected: arity,
                                found: num_args,
                            },
                            span,
                        ))
                    }
                } else if env.builtin_types.contains_key(&var.locatee) {
                    Err(Located::new(
                        Error::GenericTypeArityMismatch {
                            type_var: var.locatee,
                            expected: 0,
                            found: num_args,
                        },
                        span,
                    ))
                } else {
                    Err(Located::new(Error::UnknownTypeVar(var.locatee), span))
                }
            }
            Self::Fun(_, _) | Self::Record(_) | Self::Variant(_) => {
                for child in self.children_mut() {
                    child.check(env)?;
                }
                Ok(())
            }
        }
    }
}

impl RcType {
    fn weak_normalize_env(&self, env: &Env) -> Self {
        self.weak_normalize(&env.type_defs)
    }
}

impl LExpr {
    fn check(&mut self, env: &Env, expected: &RcType) -> Result<(), LError> {
        self.locatee.check(self.span, env, expected)
    }

    fn infer(&mut self, env: &Env) -> Result<RcType, LError> {
        self.locatee.infer(self.span, env)
    }
}

impl Expr {
    fn check(&mut self, span: Span, env: &Env, expected: &RcType) -> Result<(), LError> {
        match self {
            Self::Lam(params, body) => {
                match expected.weak_normalize_env(env).as_ref() {
                    Type::Fun(param_types, result) if params.len() == param_types.len() => {
                        let env = &mut env.clone();
                        env.intro_params(
                            // TODO(MH): Replace `x` with a pattern once
                            // https://github.com/rust-lang/rust/issues/68354
                            // has been stabilized.
                            params.iter_mut().zip(param_types.iter()).map(|mut x| {
                                let (var, opt_type_ann) = &mut x.0;
                                let expected = x.1;
                                if let Some(type_ann) = opt_type_ann {
                                    type_ann.check(env)?;
                                    let found = RcType::from_lsyntax(type_ann);
                                    if !found.equiv(expected, &env.type_defs) {
                                        return Err(Located::new(
                                            Error::ParamTypeMismatch {
                                                param: var.locatee,
                                                found,
                                                expected: expected.clone(),
                                            },
                                            type_ann.span,
                                        ));
                                    }
                                    Ok((*var, found))
                                } else {
                                    *opt_type_ann =
                                        Some(Located::new(expected.to_syntax(), var.span));
                                    Ok((*var, expected.clone()))
                                }
                            }),
                            |env| body.check(env, result),
                        )
                    }
                    _ => Err(Located::new(
                        Error::BadLam(expected.clone(), params.len()),
                        span,
                    )),
                }
            }
            Self::Let(binder, opt_type_ann, bindee, body) => {
                let binder_typ = check_let_bindee(env, binder, opt_type_ann, bindee)?;
                env.intro_binder(binder, binder_typ, |env| body.check(env, expected))
            }
            Self::If(cond, then, elze) => {
                cond.check(env, &RcType::new(Type::Bool))?;
                then.check(env, &expected)?;
                elze.check(env, &expected)?;
                Ok(())
            }
            Self::Variant(constr, opt_payload) => match expected.weak_normalize_env(env).as_ref() {
                Type::Variant(cons) => {
                    if let Some(opt_typ) = find_by_key(&cons, constr) {
                        match (opt_payload, opt_typ) {
                            (None, None) => Ok(()),
                            (Some(payload), Some(typ)) => payload.check(env, typ),
                            (opt_payload, opt_typ) => LError::variant_payload(
                                opt_payload,
                                opt_typ,
                                expected,
                                *constr,
                                span,
                            ),
                        }
                    } else {
                        Err(Located::new(
                            Error::BadVariantConstr(expected.clone(), *constr),
                            span,
                        ))
                    }
                }
                _ => Err(Located::new(
                    Error::UnexpectedVariantType(expected.clone(), *constr),
                    span,
                )),
            },
            Self::Match(scrut, branches) => {
                let branches = check_match_patterns(env, scrut, branches)?;
                for (binder, body) in branches {
                    env.intro_opt_binder(binder, |env| body.check(env, expected))?;
                }
                Ok(())
            }
            Self::Error
            | Self::Var(_)
            | Self::Num(_)
            | Self::Bool(_)
            | Self::App(_, _)
            | Self::BinOp(_, _, _)
            | Self::FuncInst(_, _)
            | Self::Record(_)
            | Self::Proj(_, _) => {
                let found = self.infer(span, env)?;
                if found.equiv(expected, &env.type_defs) {
                    Ok(())
                } else {
                    Err(Located::new(
                        Error::TypeMismatch {
                            found: found.clone(),
                            expected: expected.clone(),
                        },
                        span,
                    ))
                }
            }
        }
    }

    fn infer(&mut self, span: Span, env: &Env) -> Result<RcType, LError> {
        match self {
            Self::Error => Ok(RcType::new(Type::Error)),
            Self::Var(var) => {
                if let Some(found) = env.expr_vars.get(var) {
                    Ok(found.clone())
                } else if let Some(TypeScheme { params, body }) = env.func_sigs.get(var) {
                    let arity = params.len();
                    if arity == 0 {
                        *self = Self::FuncInst(Located::new(*var, span), vec![]);
                        Ok(body.clone())
                    } else {
                        Err(Located::new(
                            Error::GenericFuncArityMismatch {
                                expr_var: *var,
                                expected: arity,
                                found: 0,
                            },
                            span,
                        ))
                    }
                } else {
                    Err(Located::new(Error::UnknownExprVar(*var), span))
                }
            }
            Self::Num(_) => Ok(RcType::new(Type::Int)),
            Self::Bool(_) => Ok(RcType::new(Type::Bool)),
            Self::Lam(params, body) => {
                let mut param_types = Vec::with_capacity(params.len());
                let result = env.intro_params(
                    params.iter_mut().map(|(var, opt_typ)| {
                        if let Some(typ) = opt_typ {
                            typ.check(env)?;
                            let typ = RcType::from_lsyntax(typ);
                            param_types.push(typ.clone());
                            Ok((*var, typ))
                        } else {
                            Err(var.map(Error::ParamNeedsType))
                        }
                    }),
                    |env| body.infer(env),
                )?;
                Ok(RcType::new(Type::Fun(param_types, result)))
            }
            Self::App(func, args) => {
                let func_type = func.infer(env)?;
                let num_args = args.len();
                match func_type.weak_normalize_env(env).as_ref() {
                    Type::Fun(params, result) if params.len() == num_args => {
                        for (arg, typ) in args.iter_mut().zip(params.iter()) {
                            arg.check(env, typ)?;
                        }
                        Ok(result.clone())
                    }
                    _ => {
                        let func = match func.locatee {
                            Expr::Var(var) => Some(var),
                            Expr::FuncInst(func, _) => Some(func.locatee),
                            _ => None,
                        };
                        Err(Located::new(
                            Error::BadApp {
                                func,
                                func_type,
                                num_args,
                            },
                            span,
                        ))
                    }
                }
            }
            Self::BinOp(lhs, op, rhs) => match op {
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div => {
                    let int = RcType::new(Type::Int);
                    lhs.check(env, &int)?;
                    rhs.check(env, &int)?;
                    Ok(int)
                }
                OpCode::Equals
                | OpCode::NotEq
                | OpCode::Less
                | OpCode::LessEq
                | OpCode::Greater
                | OpCode::GreaterEq => {
                    let typ = lhs.infer(env)?;
                    rhs.check(env, &typ)?;
                    Ok(RcType::new(Type::Bool))
                }
            },
            Self::FuncInst(var, types) => {
                let num_types = types.len();
                for typ in types.iter_mut() {
                    typ.check(env)?;
                }
                if env.expr_vars.contains_key(&var.locatee) {
                    Err(Located::new(
                        Error::GenericFuncArityMismatch {
                            expr_var: var.locatee,
                            expected: 0,
                            found: num_types,
                        },
                        span,
                    ))
                } else if let Some(scheme) = env.func_sigs.get(&var.locatee) {
                    let arity = scheme.params.len();
                    if arity == num_types && num_types > 0 {
                        let types = types.iter().map(RcType::from_lsyntax).collect();
                        Ok(scheme.instantiate(&types))
                    } else {
                        Err(Located::new(
                            Error::GenericFuncArityMismatch {
                                expr_var: var.locatee,
                                expected: arity,
                                found: num_types,
                            },
                            span,
                        ))
                    }
                } else {
                    Err(Located::new(Error::UnknownExprVar(var.locatee), var.span))
                }
            }
            Self::Let(binder, opt_type_ann, bindee, body) => {
                let binder_typ = check_let_bindee(env, binder, opt_type_ann, bindee)?;
                env.intro_binder(binder, binder_typ, |env| body.infer(env))
            }
            Self::If(cond, then, elze) => {
                cond.check(env, &RcType::new(Type::Bool))?;
                let typ = then.infer(env)?;
                elze.check(env, &typ)?;
                Ok(typ)
            }
            Self::Record(fields) => {
                let fields = fields
                    .iter_mut()
                    .map(|(name, expr)| Ok((name.locatee, expr.infer(env)?)))
                    .collect::<Result<_, _>>()?;
                Ok(RcType::new(Type::Record(fields)))
            }
            Self::Proj(record, field) => {
                let record_type = record.infer(env)?;
                let field = field.locatee;
                match record_type.weak_normalize_env(env).as_ref() {
                    Type::Record(fields) => {
                        if let Some(field_typ) = find_by_key(&fields, &field) {
                            Ok(field_typ.clone())
                        } else {
                            Err(Located::new(
                                Error::BadRecordProj { record_type, field },
                                span,
                            ))
                        }
                    }
                    _ => Err(Located::new(
                        Error::BadRecordProj { record_type, field },
                        span,
                    )),
                }
            }
            Self::Match(scrut, branches) => {
                let branches = check_match_patterns(env, scrut, branches)?;
                let mut iter = branches.into_iter();
                let (binder, body) = iter
                    .next()
                    .expect("IMPOSSIBLE: check_match_pattern ensures we have at least one branch");
                let body_type = env.intro_opt_binder(binder, |env| body.infer(env))?;
                for (binder, body) in iter {
                    env.intro_opt_binder(binder, |env| body.check(env, &body_type))?;
                }
                Ok(body_type)
            }
            Self::Variant(_, _) => Err(Located::new(Error::TypeAnnsNeeded, span)),
        }
    }
}

impl Env {
    fn new() -> Self {
        let mut builtin_types = collections::HashMap::new();
        builtin_types.insert(
            TypeVar::new("Int"),
            Box::new(|| syntax::Type::Int) as Box<dyn Fn() -> syntax::Type>,
        );
        builtin_types.insert(
            TypeVar::new("Bool"),
            Box::new(|| syntax::Type::Bool) as Box<dyn Fn() -> syntax::Type>,
        );
        Self {
            builtin_types: Rc::new(builtin_types),
            type_defs: Rc::new(collections::HashMap::new()),
            func_sigs: Rc::new(collections::HashMap::new()),
            type_vars: im::HashSet::new(),
            expr_vars: im::HashMap::new(),
        }
    }

    fn intro_binder<F, R>(&self, var: &LExprVar, typ: RcType, f: F) -> Result<R, LError>
    where
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        let mut env = self.clone();
        env.expr_vars.insert(var.locatee, typ);
        f(&env)
    }

    fn intro_params<I, F, R>(&self, params: I, f: F) -> Result<R, LError>
    where
        I: IntoIterator<Item = Result<(LExprVar, RcType), LError>>,
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        let mut seen = std::collections::HashMap::new();
        let mut env = self.clone();
        for binder_or_err in params {
            let (var, typ) = binder_or_err?;
            if let Some(&original) = seen.get(&var.locatee) {
                return Err(Located::new(
                    Error::DuplicateParam {
                        var: var.locatee,
                        original,
                    },
                    var.span,
                ));
            } else {
                seen.insert(var.locatee, var.span);
                env.expr_vars.insert(var.locatee, typ.clone());
            }
        }
        f(&env)
    }

    fn intro_opt_binder<F, R>(&self, binder: Option<(&LExprVar, RcType)>, f: F) -> Result<R, LError>
    where
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        if let Some((var, typ)) = binder {
            self.intro_binder(var, typ, f)
        } else {
            f(self)
        }
    }
}

impl LTypeVar {
    fn check_unique<'a, I: Iterator<Item = &'a LTypeVar>>(iter: I) -> Result<(), LError> {
        if let Some((span, lvar)) = find_duplicate(iter.map(Located::as_ref)) {
            Err(Located::new(
                Error::DuplicateTypeVar {
                    var: *lvar.locatee,
                    original: span,
                },
                lvar.span,
            ))
        } else {
            Ok(())
        }
    }
}

fn check_let_bindee(
    env: &Env,
    binder: &LExprVar,
    opt_type_ann: &mut Option<syntax::LType>,
    bindee: &mut LExpr,
) -> Result<RcType, LError> {
    if let Some(type_ann) = opt_type_ann {
        type_ann.check(env)?;
        let typ = RcType::from_lsyntax(type_ann);
        bindee.check(env, &typ)?;
        Ok(typ)
    } else {
        let typ = bindee.infer(env)?;
        *opt_type_ann = Some(Located::new(typ.to_syntax(), binder.span));
        Ok(typ)
    }
}

fn check_match_patterns<'a>(
    env: &Env,
    scrut: &mut LExpr,
    branches: &'a mut [Branch],
) -> Result<Vec<(Option<(&'a LExprVar, RcType)>, &'a mut LExpr)>, LError> {
    let scrut_type = scrut.infer(env)?;
    match scrut_type.weak_normalize_env(env).as_ref() {
        Type::Variant(constrs) => {
            if branches.len() > 0 {
                branches
                    .iter_mut()
                    .map(|Branch { pattern, body }| {
                        let constr = pattern.locatee.constr;
                        if let Some(opt_typ) = find_by_key(constrs, &constr) {
                            match (&pattern.locatee.binder, opt_typ) {
                                (None, None) => Ok((None, body)),
                                (Some(var), Some(typ)) => Ok((Some((var, typ.clone())), body)),
                                (opt_payload, opt_typ) => LError::variant_payload(
                                    opt_payload,
                                    opt_typ,
                                    &scrut_type,
                                    constr,
                                    pattern.span,
                                ),
                            }
                        } else {
                            Err(Located::new(
                                Error::BadBranch(scrut_type.clone(), constr),
                                pattern.span,
                            ))
                        }
                    })
                    .collect::<Result<_, _>>()
            } else {
                Err(Located::new(Error::EmptyMatch, scrut.span))
            }
        }
        _ => Err(Located::new(Error::BadMatch(scrut_type), scrut.span)),
    }
}

fn find_duplicate<'a, T: Eq + Hash, I: Iterator<Item = Located<T>>>(
    iter: I,
) -> Option<(Span, Located<T>)> {
    let mut seen = std::collections::HashMap::new();
    for lvar in iter {
        if let Some(span) = seen.get(&lvar.locatee) {
            return Some((*span, lvar));
        } else {
            seen.insert(lvar.locatee, lvar.span);
        }
    }
    None
}

fn find_by_key<'a, K: Eq, V>(vec: &'a Vec<(K, V)>, key: &K) -> Option<&'a V> {
    vec.iter()
        .find_map(|(k, v)| if k == key { Some(v) } else { None })
}
