mod error;
mod info;
mod tests;
mod types;

use std::cell::Cell;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use self::error::Error;
use self::error::LError;
pub use self::info::SymbolInfo;
use self::types::FuncSig;
pub use self::types::RcType;
pub use self::types::Type;
use self::types::TypeScheme;
use self::types::UnificationVar;
use crate::diagnostic::Diagnostic;
use crate::diagnostic::Severity;
use crate::diagnostic::Source;
use crate::location::Located;
use crate::location::SourceSpan;
use crate::syntax::Branch;
use crate::syntax::Expr;
use crate::syntax::ExprVar;
use crate::syntax::FuncDecl;
use crate::syntax::LExpr;
use crate::syntax::LExprVar;
use crate::syntax::LType;
use crate::syntax::Module;
use crate::syntax::OpCode;
use crate::syntax::Pattern;
use crate::syntax::Type as SynType;
use crate::syntax::TypeDecl;
use crate::syntax::TypeVar;

type Arity = usize;

#[derive(Clone)]
struct Env {
    builtin_types: Rc<HashMap<TypeVar, Box<dyn Fn() -> SynType>>>,
    type_defs: Rc<HashMap<TypeVar, TypeScheme>>,
    func_sigs: Rc<HashMap<ExprVar, Arc<FuncSig>>>,
    type_vars: im::HashSet<TypeVar>,
    expr_vars: im::HashMap<ExprVar, (SourceSpan, RcType)>,
    symbols: Rc<Cell<Vec<SymbolInfo>>>,
    unification_var_counter: Rc<Cell<u32>>,
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
        check_unique(self.type_decls().map(|decl| decl.name.as_ref()), |var, original| {
            Error::DuplicateTypeDecl { var, original }
        })?;
        check_unique(self.func_decls().map(|decl| decl.name.as_ref()), |var, original| {
            Error::DuplicateFuncDecl { var, original }
        })?;
        let mut env = Env::new();
        env.type_defs = Rc::new(self.type_defs());
        for type_decl in self.type_decls_mut() {
            type_decl.check(&env)?;
        }
        // We reset `type_def` since the check above also updates the type with name information.
        env.type_defs = Rc::new(self.type_defs());
        for type_decl in self.type_decls() {
            type_decl.check_contractive(&env)?;
        }
        // We use `weak_normalize` in `check_polymorphic_recursion`, so we first need to check that
        // _all_ type declarations are contractive.
        for type_decl in self.type_decls() {
            type_decl.check_polymorphic_recursion(&env)?;
        }
        let func_sigs = self
            .func_decls_mut()
            .map(|decl| Ok((decl.name.locatee, Arc::new(decl.check_signature(&env)?))))
            .collect::<Result<_, _>>()?;
        env.func_sigs = Rc::new(func_sigs);
        for decl in self.func_decls_mut() {
            decl.check(&env)?;
        }
        let mut symbols: Vec<SymbolInfo> = env.symbols.take();
        symbols.sort_by_key(|symbol| symbol.span().start);
        Ok(symbols)
    }

    fn type_defs(&self) -> HashMap<TypeVar, TypeScheme> {
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
        check_unique(params.iter().map(Located::as_ref), |var, original| {
            Error::DuplicateTypeVar { var, original }
        })?;
        let env = &mut env.clone();
        env.type_vars = params.iter().map(|param| param.locatee).collect();
        body.check(env)
    }

    fn check_contractive(&self, env: &Env) -> Result<(), LError> {
        let mut seen = HashSet::new();
        let mut current = self.name.locatee;
        while let Type::SynApp(name, _) = env
            .type_defs
            .get(&current)
            .expect("Unresolvable type declaration after successful name resolution.")
            .body
            .as_ref()
        {
            if *name == self.name.locatee {
                return Err(Located::new(
                    Error::NonContractiveTypeDecl(self.name.locatee),
                    self.name.span,
                ));
            }
            if !seen.insert(*name) {
                // Type declaration is not contractive but not the root of the problem.
                break;
            }
            current = *name;
        }
        Ok(())
    }

    /// Check whether the type declaration is _not_ polymorphically recursive assuming that all
    /// other type declarations are not. We use this as a conservative approximation of regularity.
    /// (A type is regular if its tree representation has only finitely many different subtrees.)
    ///
    /// A type declaration `F<X1, ..., Xn>` is polymorphically recursive if its expansion contains
    /// an instantiation `F<T1, ..., Tn>` where `Ti != Xi` for some `i`. For example
    ///
    /// ```homer
    /// type F<X> = [A | B(F<List<X>>)]
    /// type G<Y, Z> = [C | D(G<Z, Y>)]
    /// ```
    ///
    /// are polymorphically recursive whereas
    ///
    /// ```homer
    /// type F<X, Y> = [A | B(G<Y, X>)]
    /// type G<U, V> = [C | D(F<V, U>)]
    /// ```
    ///
    /// are not.
    ///
    /// There are more permissive conservative approximations but "polymorphic recursion" is a well
    /// established term for functions, so we use that for now.
    fn check_polymorphic_recursion(&self, env: &Env) -> Result<(), LError> {
        // Check if the type tree for `typ` has polymorphic instantiation of the type named `root`.
        // `seen` keeps track of the names of the type declarations expanded along the way.
        fn has_poly_inst(
            root: TypeVar,
            env: &Env,
            typ: &RcType,
            seen: &mut HashSet<TypeVar>,
        ) -> bool {
            if let Type::SynApp(name, args) = &**typ {
                let scheme = &env
                    .type_defs
                    .get(name)
                    .expect("Unresolvable type declaration after successful name resolution.");
                if *name == root {
                    assert_eq!(scheme.params.len(), args.len());
                    scheme.params.iter().zip(args.iter()).any(|(param, arg)| {
                        // We normalize to support cases where `arg` the is an alias expanding to a
                        // type var.
                        !matches!(&*arg.weak_normalize_env(env), Type::Var(var) if var == param)
                    })
                } else if seen.insert(*name) {
                    let res = has_poly_inst(root, env, &scheme.instantiate(args), seen);
                    // It is important that we remove `name` again since sibling instantiations
                    // of the type alias `name` can have different arguments and lead to different
                    // instantiations of `root`.
                    seen.remove(name);
                    res
                } else {
                    // We've expanded the type alias `name` on the path from the root of the
                    // type tree to the current subtree. Since `name` is assumed to not be
                    // polymorphically recursive, we won't find a polymorphic instantiation of
                    // `root` we don't find under the previous expansion too.
                    false
                }
            } else {
                typ.children().any(|child| has_poly_inst(root, env, child, seen))
            }
        }

        let typ = &env
            .type_defs
            .get(&self.name.locatee)
            .expect("Unresolvable type declaration after successful name resolution.")
            .body;
        if has_poly_inst(self.name.locatee, env, typ, &mut HashSet::new()) {
            Err(Located::new(
                Error::PolymorphicallyRecursiveTypeDecl(self.name.locatee),
                self.name.span,
            ))
        } else {
            Ok(())
        }
    }
}

impl FuncDecl {
    fn check_signature(&mut self, env: &Env) -> Result<FuncSig, LError> {
        let Self { name, type_params, expr_params, return_type, body: _ } = self;
        check_unique(type_params.iter().map(Located::as_ref), |var, original| {
            Error::DuplicateTypeVar { var, original }
        })?;
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

impl SynType {
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
            Self::Fun(_, _) => {
                for child in self.children_mut() {
                    child.check(env)?;
                }
                Ok(())
            }
            Self::Record(fields) => {
                check_unique(fields.iter().map(|(field, _)| field.as_ref()), |field, original| {
                    Error::DuplicateRecordField { field, original }
                })?;
                for child in self.children_mut() {
                    child.check(env)?;
                }
                Ok(())
            }
            Self::Variant(constrs) => {
                check_unique(
                    constrs.iter().map(|(constr, _)| constr.as_ref()),
                    |constr, original| Error::DuplicateVariantConstr { constr, original },
                )?;
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
            Self::Lam(params, body) => match expected.weak_normalize_env(env).as_ref() {
                Type::Fun(param_types, result) if params.len() == param_types.len() => {
                    let env = &mut env.clone();
                    env.intro_params(
                        params.iter_mut().zip(param_types.iter()).map(
                            |((var, opt_type_ann), expected)| {
                                if let Some(type_ann) = opt_type_ann {
                                    type_ann.check(env)?;
                                    let found = RcType::from_lsyntax(type_ann);
                                    if !found.equiv(expected, &env.type_defs) {
                                        Err(Located::new(
                                            Error::ParamTypeMismatch {
                                                param: var.locatee,
                                                found,
                                                expected: expected.clone(),
                                            },
                                            type_ann.span,
                                        ))
                                    } else {
                                        Ok((*var, found))
                                    }
                                } else {
                                    *opt_type_ann = Some(expected.as_inferred(var));
                                    Ok((*var, expected.clone()))
                                }
                            },
                        ),
                        |env| body.check(env, result),
                    )
                }
                _ => Err(Located::new(Error::BadLam(expected.clone(), params.len()), span)),
            },
            Self::AppFun(fun, opt_types, args) => {
                check_app_fun(env, span, fun, opt_types, args, |result| {
                    found_vs_expected(env, span, result, expected)
                })
            }
            Self::Let(binder, opt_type_ann, bindee, tail) => {
                let binder_typ = check_let_bindee(env, binder, opt_type_ann, bindee)?;
                env.intro_binder(binder, binder_typ, |env| tail.check(env, expected))
            }
            Self::If(cond, then, elze) => {
                cond.check(env, &RcType::new(Type::Bool))?;
                then.check(env, expected)?;
                elze.check(env, expected)?;
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
            | Self::BinOp(_, _, _)
            // TODO(MH): Pushing the expected type into the fields gives better inference.
            | Self::Record(_)
            | Self::Proj(_, _, _) => {
                // This is subsumption.
                let found = self.infer_resolved(span, env)?;
                found_vs_expected(env, span, found, expected)
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
                check_app_fun(env, span, fun, opt_types, args, Ok)
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
                    Err(Located::new(Error::UnknownExprVar { expr_var: *var, is_func }, span))
                }
            }
            Self::AppClo(clo, args) => {
                if env.expr_vars.contains_key(&clo.locatee) {
                    Ok(())
                } else if env.func_sigs.contains_key(&clo.locatee) {
                    *self = Self::AppFun(*clo, None, std::mem::take(args));
                    Ok(())
                } else {
                    Err(Located::new(
                        Error::UnknownExprVar { expr_var: clo.locatee, is_func: false },
                        clo.span,
                    ))
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
                    Err(Located::new(
                        Error::UnknownExprVar { expr_var: fun.locatee, is_func: false },
                        fun.span,
                    ))
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
        let mut builtin_types = HashMap::new();
        builtin_types
            .insert(TypeVar::new("Int"), Box::new(|| SynType::Int) as Box<dyn Fn() -> SynType>);
        builtin_types
            .insert(TypeVar::new("Bool"), Box::new(|| SynType::Bool) as Box<dyn Fn() -> SynType>);
        Self {
            builtin_types: Rc::new(builtin_types),
            type_defs: Rc::new(HashMap::new()),
            func_sigs: Rc::new(HashMap::new()),
            type_vars: im::HashSet::new(),
            expr_vars: im::HashMap::new(),
            symbols: Rc::new(Cell::new(Vec::new())),
            unification_var_counter: Rc::new(Cell::new(1)),
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
        let mut seen = HashMap::new();
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
        let mut infos = self.symbols.take();
        infos.push(info);
        self.symbols.set(infos);
    }

    fn fresh_unification_type(&self) -> RcType {
        let name = self.unification_var_counter.get();
        self.unification_var_counter.set(name + 1);
        RcType::new(Type::UnificationVar(UnificationVar::new_free(name)))
    }
}

fn found_vs_expected(
    env: &Env,
    span: SourceSpan,
    found: RcType,
    expected: &RcType,
) -> Result<(), LError> {
    if found.equiv(expected, &env.type_defs) {
        Ok(())
    } else {
        Err(Located::new(Error::TypeMismatch { found, expected: expected.clone() }, span))
    }
}

fn check_let_bindee(
    env: &Env,
    binder: &LExprVar,
    opt_type_ann: &mut Option<Located<SynType>>,
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

fn check_app_fun<R, F: FnOnce(RcType) -> Result<R, LError>>(
    env: &Env,
    span: SourceSpan,
    fun: &LExprVar,
    opt_types: &mut Option<Vec<LType>>,
    args: &mut [LExpr],
    check_or_pass: F,
) -> Result<R, LError> {
    if let Some(types) = opt_types {
        for typ in types.iter_mut() {
            typ.check(env)?;
        }
    }
    let func_sig = env.func_sigs.get(&fun.locatee).unwrap();
    // Name resolution only passes `f@<>(...)` on when `f` is a polymorphic function.
    if matches!(opt_types, Some(types) if types.is_empty()) {
        assert!(!func_sig.params.is_empty());
    }
    env.add_symbol(SymbolInfo::FuncRef { var: *fun, def: Arc::clone(func_sig) });
    let num_type_params = func_sig.type_params.len();
    let num_types = opt_types.as_ref().map_or(num_type_params, |types| types.len());
    if num_types == num_type_params {
        let types: Vec<_> = match opt_types {
            Some(types) => types.iter().map(RcType::from_lsyntax).collect(),
            None => {
                let (types, syntax_types) = (0..num_types)
                    .map(|_| {
                        let typ = env.fresh_unification_type();
                        (typ.clone(), typ.as_inferred(fun))
                    })
                    .unzip();
                *opt_types = Some(syntax_types);
                types
            }
        };
        let (params, result) = func_sig.instantiate(&types);
        if args.len() == params.len() {
            // In check mode, we get better inference if we first unify the expected type with the
            // result type before we check the arguments.
            let result = check_or_pass(result);
            for (arg, typ) in args.iter_mut().zip(params.iter()) {
                arg.check(env, typ)?;
            }
            result
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
                let mut matched = BTreeSet::new();
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

fn check_unique<'a, T: Copy + Eq + Hash + 'a>(
    iter: impl Iterator<Item = Located<&'a T>>,
    duplicate_err: impl Fn(T, SourceSpan) -> Error,
) -> Result<(), LError> {
    let mut seen = HashMap::new();
    for lvar in iter {
        if let Some(first_span) = seen.get(&lvar.locatee) {
            return Err(Located::new(duplicate_err(*lvar.locatee, *first_span), lvar.span));
        } else {
            seen.insert(lvar.locatee, lvar.span);
        }
    }
    Ok(())
}
