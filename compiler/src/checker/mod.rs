use crate::*;
use diagnostic::*;
use error::{Error, LError};
use location::{Located, SourceSpan};
use std::cell::Cell;
use std::collections;
use std::hash::Hash;
use std::rc::Rc;
use syntax::*;
use types::*;

mod error;
pub mod info;
mod types;

type Arity = usize;

pub use info::SymbolInfo;
pub use types::{RcType, SynType, Type};

#[derive(Clone)]
struct Env {
    builtin_types: Rc<collections::HashMap<TypeVar, Box<dyn Fn() -> syntax::Type>>>,
    type_defs: Rc<collections::HashMap<TypeVar, TypeScheme>>,
    func_sigs: Rc<collections::HashMap<ExprVar, Rc<FuncSig>>>,
    type_vars: im::HashSet<TypeVar>,
    expr_vars: im::HashMap<ExprVar, (SourceSpan, RcType)>,
    symbols: Rc<Cell<Vec<SymbolInfo>>>,
}

impl Module {
    pub fn check(&self) -> Result<(Self, Vec<SymbolInfo>), Diagnostic> {
        let mut module = self.clone();
        match module.check_impl() {
            Ok(symbols) => Ok((module, symbols)),
            Err(error) => Err(Diagnostic {
                span: error.span,
                severity: Severity::Error,
                source: Source::Checker,
                message: format!("{}", error.locatee),
            }),
        }
    }

    fn check_impl(&mut self) -> Result<Vec<SymbolInfo>, LError> {
        if let Some((span, name)) = find_duplicate(self.type_decls().map(|decl| decl.name.as_ref()))
        {
            return Err(Located::new(
                Error::DuplicateTypeDecl { var: *name.locatee, original: span },
                name.span,
            ));
        }
        if let Some((span, name)) = find_duplicate(self.func_decls().map(|decl| decl.name.as_ref()))
        {
            return Err(Located::new(
                Error::DuplicateFuncDecl { var: *name.locatee, original: span },
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
            .map(|decl| Ok((decl.name.locatee, Rc::new(decl.check_signature(&env)?))))
            .collect::<Result<_, _>>()?;
        env.func_sigs = Rc::new(func_sigs);
        for decl in self.func_decls_mut() {
            decl.check(&env)?;
        }
        let mut symbols = env.symbols.take();
        symbols.sort_by_key(|symbol| symbol.span().start);
        Ok(symbols)
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
        let Self { name: _, params, body } = self;
        LTypeVar::check_unique(params.iter())?;
        let env = &mut env.clone();
        env.type_vars = params.iter().map(|param| param.locatee).collect();
        body.check(env)
    }
}

impl FuncDecl {
    fn check_signature(&mut self, env: &Env) -> Result<FuncSig, LError> {
        let Self { name, type_params, expr_params, return_type, body: _ } = self;
        LTypeVar::check_unique(type_params.iter())?;
        let env = &mut env.clone();
        env.type_vars = type_params.iter().map(|param| param.locatee).collect();
        for (_, typ) in expr_params.iter_mut() {
            typ.check(env)?;
        }
        return_type.check(env)?;
        Ok(FuncSig {
            name: *name,
            type_params: type_params.iter().map(|param| param.locatee).collect(),
            params: expr_params
                .iter()
                .map(|(var, typ)| (var.locatee, RcType::from_lsyntax(typ)))
                .collect(),
            result: RcType::from_lsyntax(return_type),
        })
    }

    fn check(&mut self, env: &Env) -> Result<(), LError> {
        let Self { name: _, type_params, expr_params, return_type, body } = self;
        let env = &mut env.clone();
        env.type_vars = type_params.iter().map(|param| param.locatee).collect();
        env.intro_params(
            expr_params.iter().map(|(var, typ)| Ok((*var, RcType::from_lsyntax(typ)))),
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
    fn check(&mut self, span: SourceSpan, env: &Env) -> Result<(), LError> {
        match self {
            Self::Error => Ok(()),
            Self::Int => panic!("Int in Type.check"),
            Self::Bool => panic!("Bool in Type.check"),
            Self::Var(var) => {
                let var = &var.locatee;
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
            Self::Inferred(_) => panic!("IMPOSSIBLE: only introduced by type checker"),
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
    fn check(&mut self, span: SourceSpan, env: &Env, expected: &RcType) -> Result<(), LError> {
        self.resolve(span, env)?;
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
                                    *opt_type_ann = Some(expected.as_inferred(var));
                                    Ok((*var, expected.clone()))
                                }
                            }),
                            |env| body.check(env, result),
                        )
                    }
                    _ => Err(Located::new(Error::BadLam(expected.clone(), params.len()), span)),
                }
            }
            Self::Let(binder, opt_type_ann, bindee, tail) => {
                let binder_typ = check_let_bindee(env, binder, opt_type_ann, bindee)?;
                env.intro_binder(binder, binder_typ, |env| tail.check(env, expected))
            }
            Self::If(cond, then, elze) => {
                cond.check(env, &RcType::new(Type::Bool))?;
                then.check(env, &expected)?;
                elze.check(env, &expected)?;
                Ok(())
            }
            Self::Variant(constr, rank, payload) => match expected.weak_normalize_env(env).as_ref()
            {
                Type::Variant(constrs) => {
                    if let Some(constr_rank) = constrs.iter().position(|x| x.0 == *constr) {
                        *rank = Some(constr_rank as u32);
                        match (payload, &constrs[constr_rank].1) {
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
                        Err(Located::new(Error::BadVariantConstr(expected.clone(), *constr), span))
                    }
                }
                _ => {
                    Err(Located::new(Error::UnexpectedVariantType(expected.clone(), *constr), span))
                }
            },
            Self::Match(scrut, branches) => {
                let branches = check_match_patterns(env, scrut, branches)?;
                for (binder, rhs) in branches {
                    env.intro_opt_binder(binder, |env| rhs.check(env, expected))?;
                }
                Ok(())
            }
            Self::Error
            | Self::Var(_)
            | Self::Num(_)
            | Self::Bool(_)
            | Self::AppClo(_, _)
            | Self::AppFun(_, _, _)
            | Self::BinOp(_, _, _)
            | Self::Record(_)
            | Self::Proj(_, _, _) => {
                let found = self.infer_resolved(span, env)?;
                if found.equiv(expected, &env.type_defs) {
                    Ok(())
                } else {
                    Err(Located::new(
                        Error::TypeMismatch { found, expected: expected.clone() },
                        span,
                    ))
                }
            }
        }
    }

    fn infer(&mut self, span: SourceSpan, env: &Env) -> Result<RcType, LError> {
        self.resolve(span, env)?;
        self.infer_resolved(span, env)
    }

    fn infer_resolved(&mut self, span: SourceSpan, env: &Env) -> Result<RcType, LError> {
        match self {
            Self::Error => Ok(RcType::new(Type::Error)),
            Self::Var(var) => {
                let (def, typ) = env.expr_vars.get(&var.locatee).unwrap();
                env.add_symbol(SymbolInfo::ExprVar { var: *var, typ: typ.clone(), def: *def });
                Ok(typ.clone())
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
            Self::AppClo(clo, args) => {
                let (def, clo_type) = env.expr_vars.get(&clo.locatee).unwrap();
                env.add_symbol(SymbolInfo::ExprVar { var: *clo, typ: clo_type.clone(), def: *def });
                match clo_type.weak_normalize_env(env).as_ref() {
                    Type::Fun(params, result) if args.len() == params.len() => {
                        for (arg, typ) in args.iter_mut().zip(params.iter()) {
                            arg.check(env, typ)?;
                        }
                        Ok(result.clone())
                    }
                    _ => Err(Located::new(
                        Error::BadApp {
                            func: Some(clo.locatee),
                            func_type: clo_type.clone(),
                            num_args: args.len(),
                        },
                        span,
                    )),
                }
            }
            Self::AppFun(fun, opt_types, args) => {
                let num_types = opt_types.as_ref().map_or(0, |types| types.len());
                if let Some(types) = opt_types {
                    for typ in types.iter_mut() {
                        typ.check(env)?;
                    }
                }
                let func_sig = env.func_sigs.get(&fun.locatee).unwrap();
                // NOTE(MH): The name resolution only passes `f@<>(...)` on
                // when `f` is a polymorphic function.
                if matches!(opt_types, Some(types) if types.is_empty()) {
                    assert!(!func_sig.params.is_empty());
                }
                env.add_symbol(SymbolInfo::FuncRef { var: *fun, def: Rc::clone(func_sig) });
                if num_types == func_sig.type_params.len() {
                    let types: Vec<_> = opt_types
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(RcType::from_lsyntax)
                        .collect();
                    let (params, result) = func_sig.instantiate(&types);
                    if args.len() == params.len() {
                        for (arg, typ) in args.iter_mut().zip(params.iter()) {
                            arg.check(env, typ)?;
                        }
                        Ok(result)
                    } else {
                        Err(Located::new(
                            Error::BadApp {
                                func: Some(fun.locatee),
                                func_type: RcType::new(Type::Fun(params, result)),
                                num_args: args.len(),
                            },
                            span,
                        ))
                    }
                } else {
                    Err(Located::new(
                        Error::GenericFuncArityMismatch {
                            expr_var: fun.locatee,
                            expected: func_sig.type_params.len(),
                            found: num_types,
                        },
                        fun.span,
                    ))
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
            Self::Let(binder, opt_type_ann, bindee, tail) => {
                let binder_typ = check_let_bindee(env, binder, opt_type_ann, bindee)?;
                env.intro_binder(binder, binder_typ, |env| tail.infer(env))
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
            Self::Proj(record, field, index) => {
                let record_type = record.infer(env)?;
                let field = field.locatee;
                match record_type.weak_normalize_env(env).as_ref() {
                    Type::Record(fields) => {
                        if let Some(field_index) = fields.iter().position(|x| x.0 == field) {
                            *index = Some(field_index as u32);
                            Ok(fields[field_index].1.clone())
                        } else {
                            Err(Located::new(Error::BadRecordProj { record_type, field }, span))
                        }
                    }
                    _ => Err(Located::new(Error::BadRecordProj { record_type, field }, span)),
                }
            }
            Self::Match(scrut, branches) => {
                let branches = check_match_patterns(env, scrut, branches)?;
                let mut iter = branches.into_iter();
                let (binder, rhs) = iter
                    .next()
                    .expect("IMPOSSIBLE: check_match_pattern ensures we have at least one branch");
                let rhs_type = env.intro_opt_binder(binder, |env| rhs.infer(env))?;
                for (binder, rhs) in iter {
                    env.intro_opt_binder(binder, |env| rhs.check(env, &rhs_type))?;
                }
                Ok(rhs_type)
            }
            Self::Variant(_, _, _) => Err(Located::new(Error::TypeAnnsNeeded, span)),
        }
    }

    fn resolve(&mut self, span: SourceSpan, env: &Env) -> Result<(), LError> {
        match self {
            Self::Var(var) => {
                let var = &var.locatee;
                if env.expr_vars.contains_key(var) {
                    Ok(())
                } else {
                    let is_func = env.func_sigs.contains_key(var);
                    Err(Located::new(Error::UnknownExprVar(*var, is_func), span))
                }
            }
            Self::AppClo(clo, args) => {
                if env.expr_vars.contains_key(&clo.locatee) {
                    Ok(())
                } else if env.func_sigs.contains_key(&clo.locatee) {
                    *self = Self::AppFun(*clo, None, std::mem::take(args));
                    Ok(())
                } else {
                    Err(Located::new(Error::UnknownExprVar(clo.locatee, false), clo.span))
                }
            }
            Self::AppFun(fun, opt_types, _args) => {
                let mismatch = || {
                    Err(Located::new(Error::BadNonGenericCall { expr_var: fun.locatee }, fun.span))
                };
                if env.expr_vars.contains_key(&fun.locatee) {
                    mismatch()
                } else if let Some(func_sig) = env.func_sigs.get(&fun.locatee) {
                    if func_sig.type_params.is_empty() && opt_types.is_some() {
                        mismatch()
                    } else {
                        Ok(())
                    }
                } else {
                    Err(Located::new(Error::UnknownExprVar(fun.locatee, false), fun.span))
                }
            }
            Self::Error
            | Self::Num(_)
            | Self::Bool(_)
            | Self::Lam(_, _)
            | Self::BinOp(_, _, _)
            | Self::Let(_, _, _, _)
            | Self::If(_, _, _)
            | Self::Record(_)
            | Self::Proj(_, _, _)
            | Self::Variant(_, _, _)
            | Self::Match(_, _) => Ok(()),
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
            symbols: Rc::new(Cell::new(Vec::new())),
        }
    }

    fn intro_binder<F, R>(&self, var: &LExprVar, typ: RcType, f: F) -> Result<R, LError>
    where
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        let mut env = self.clone();
        env.expr_vars.insert(var.locatee, (var.span, typ.clone()));
        self.add_symbol(SymbolInfo::ExprBinder { var: *var, typ });
        f(&env)
    }

    fn intro_params<I, F, R>(&self, params: I, f: F) -> Result<R, LError>
    where
        I: IntoIterator<Item = Result<(LExprVar, RcType), LError>>,
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        let mut seen = collections::HashMap::new();
        let mut env = self.clone();
        for binder_or_err in params {
            let (var, typ) = binder_or_err?;
            if let Some(&original) = seen.get(&var.locatee) {
                return Err(Located::new(
                    Error::DuplicateParam { var: var.locatee, original },
                    var.span,
                ));
            } else {
                seen.insert(var.locatee, var.span);
                env.expr_vars.insert(var.locatee, (var.span, typ.clone()));
                self.add_symbol(SymbolInfo::ExprBinder { var, typ });
            }
        }
        f(&env)
    }

    fn intro_opt_binder<F, R>(&self, binder: Option<(&LExprVar, RcType)>, f: F) -> Result<R, LError>
    where
        F: FnOnce(&Self) -> Result<R, LError>,
    {
        match binder {
            None => f(self),
            Some((var, typ)) => self.intro_binder(var, typ, f),
        }
    }

    fn add_symbol(&self, info: SymbolInfo) {
        let mut symbols = self.symbols.take();
        symbols.push(info);
        self.symbols.set(symbols);
    }
}

impl LTypeVar {
    fn check_unique<'a, I: Iterator<Item = &'a LTypeVar>>(iter: I) -> Result<(), LError> {
        if let Some((span, lvar)) = find_duplicate(iter.map(Located::as_ref)) {
            Err(Located::new(
                Error::DuplicateTypeVar { var: *lvar.locatee, original: span },
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
        *opt_type_ann = Some(typ.as_inferred(binder));
        Ok(typ)
    }
}

type TypedBranch<'a> = (Option<(&'a LExprVar, RcType)>, &'a mut LExpr);

fn check_match_patterns<'a>(
    env: &Env,
    scrut: &mut LExpr,
    branches: &'a mut [Branch],
) -> Result<Vec<TypedBranch<'a>>, LError> {
    let scrut_type = scrut.infer(env)?;
    match scrut_type.weak_normalize_env(env).as_ref() {
        Type::Variant(constrs) => {
            if !branches.is_empty() {
                let mut matched = std::collections::BTreeSet::new();
                let branches = branches
                    .iter_mut()
                    .map(|Branch { pattern, rhs }| {
                        let Pattern { constr, rank, binder } = &mut pattern.locatee;
                        if let Some(constr_rank) = constrs.iter().position(|x| x.0 == *constr) {
                            if matched.contains(&constr_rank) {
                                Err(Located::new(
                                    Error::OverlappingMatch(scrut_type.clone(), *constr),
                                    pattern.span,
                                ))
                            } else {
                                matched.insert(constr_rank);
                                *rank = Some(constr_rank as u32);
                                match (binder, &constrs[constr_rank].1) {
                                    (None, None) => Ok((None, rhs)),
                                    (Some(binder), Some(typ)) => {
                                        Ok((Some((&*binder, typ.clone())), rhs))
                                    }
                                    (opt_binder, opt_typ) => LError::variant_payload(
                                        opt_binder,
                                        opt_typ,
                                        &scrut_type,
                                        *constr,
                                        pattern.span,
                                    ),
                                }
                            }
                        } else {
                            Err(Located::new(
                                Error::BadBranch(scrut_type.clone(), *constr),
                                pattern.span,
                            ))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                if matched.len() < constrs.len() {
                    let missing_rank = (0..constrs.len())
                        .find(|rank| !matched.contains(rank))
                        .expect("IMPOSSIBLE");
                    let missing_constr = constrs[missing_rank].0;
                    Err(Located::new(
                        Error::NonExhaustiveMatch(scrut_type, missing_constr),
                        scrut.span,
                    ))
                } else {
                    Ok(branches)
                }
            } else {
                Err(Located::new(Error::EmptyMatch, scrut.span))
            }
        }
        _ => Err(Located::new(Error::BadMatch(scrut_type), scrut.span)),
    }
}

fn find_duplicate<T: Eq + Hash, I: Iterator<Item = Located<T>>>(
    iter: I,
) -> Option<(SourceSpan, Located<T>)> {
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
