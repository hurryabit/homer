use super::*;

impl Module {
    pub fn type_decls(&self) -> impl Iterator<Item = &TypeDecl> {
        self.decls.iter().filter_map(|decl| match decl {
            Decl::Type(type_decl) => Some(type_decl),
            Decl::Func(_) => None,
        })
    }

    pub fn type_decls_mut(&mut self) -> impl Iterator<Item = &mut TypeDecl> {
        self.decls.iter_mut().filter_map(|decl| match decl {
            Decl::Type(type_decl) => Some(type_decl),
            Decl::Func(_) => None,
        })
    }

    pub fn func_decls(&self) -> impl Iterator<Item = &FuncDecl> {
        self.decls.iter().filter_map(|decl| match decl {
            Decl::Func(func) => Some(func),
            Decl::Type(_) => None,
        })
    }

    pub fn func_decls_mut(&mut self) -> impl Iterator<Item = &mut FuncDecl> {
        self.decls.iter_mut().filter_map(|decl| match decl {
            Decl::Func(func) => Some(func),
            Decl::Type(_) => None,
        })
    }
}

impl LType {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LType> {
        self.locatee.children_mut()
    }
}

impl Type {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LType> {
        use genawaiter::{rc::gen, yield_};
        gen!({
            match self {
                Self::Error => {}
                Self::Var(_) | Self::Int | Self::Bool => {}
                Self::SynApp(syn, args) => {
                    let _: &LTypeVar = syn; // We want this to break if change the type of `syn`.
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
                Self::Inferred(_) => {}
            }
        })
        .into_iter()
    }
}

impl LExpr {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LExpr> {
        self.locatee.children_mut()
    }
}

impl Expr {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LExpr> {
        use genawaiter::{rc::gen, yield_};
        gen!({
            match self {
                Self::Error => {}
                Self::Var(_) | Self::Num(_) | Self::Bool(_) => {}
                Self::Lam(_params, body) => {
                    yield_!(body.as_mut());
                }
                Self::AppClo(clo, args) => {
                    let _: &LExprVar = clo; // We want this to fail if we change the type of `clo`.
                    for arg in args {
                        yield_!(arg);
                    }
                }
                Self::AppFun(fun, _types, args) => {
                    let _: &LExprVar = fun; // We want this to fail if we change the type of `fun`.
                    for arg in args {
                        yield_!(arg);
                    }
                }
                Self::BinOp(lhs, _opcode, rhs) => {
                    yield_!(lhs);
                    yield_!(rhs);
                }
                Self::Let(_binder, _type, bindee, tail) => {
                    yield_!(bindee);
                    yield_!(tail);
                }
                Self::If(cond, then, elze) => {
                    yield_!(cond);
                    yield_!(then);
                    yield_!(elze);
                }
                Self::Record(fields) => {
                    for (_name, expr) in fields {
                        yield_!(expr);
                    }
                }
                Self::Proj(record, _field, _index) => {
                    yield_!(record);
                }
                Self::Variant(_constr, _rank, payload) => {
                    if let Some(payload) = payload {
                        yield_!(payload);
                    }
                }
                Self::Match(scrut, branches) => {
                    yield_!(scrut);
                    for branch in branches {
                        yield_!(&mut branch.rhs);
                    }
                }
            }
        })
        .into_iter()
    }

    pub fn for_each_child<F: FnMut(&LExpr)>(&self, f: &mut F) {
        match self {
            Self::Error | Self::Var(_) | Self::Num(_) | Self::Bool(_) => {}
            Self::Lam(_params, body) => f(body),
            Self::AppClo(clo, args) => {
                let _: &LExprVar = clo; // We want this to fail if we change the type of `clo`.
                args.iter().for_each(f);
            }
            Self::AppFun(fun, _types, args) => {
                let _: &LExprVar = fun; // We want this to fail if we change the type of `fun`.
                args.iter().for_each(f)
            }
            Self::BinOp(lhs, _opcode, rhs) => {
                f(lhs);
                f(rhs);
            }
            Self::Let(_binder, _type, bindee, tail) => {
                f(bindee);
                f(tail);
            }
            Self::If(cond, then, elze) => {
                f(cond);
                f(then);
                f(elze);
            }
            Self::Record(fields) => {
                fields.iter().map(|x| &x.1).for_each(f);
            }
            Self::Proj(record, _field, _index) => {
                f(record);
            }
            Self::Variant(_constr, _rank, payload) => {
                payload.iter().map(|x| x.as_ref()).for_each(f);
            }
            Self::Match(scrut, branches) => {
                f(scrut);
                branches.iter().map(|b| &b.rhs).for_each(f);
            }
        }
    }
}

pub enum ExprRef {
    Var(LExprVar),
    Clo(LExprVar),
    Fun(LExprVar),
}

impl Expr {
    pub fn for_each_ref<F: FnMut(ExprRef)>(&self, f: &mut F) {
        match self {
            Expr::Var(var) => f(ExprRef::Var(*var)),
            Expr::AppClo(clo, _args) => f(ExprRef::Clo(*clo)),
            Expr::AppFun(fun, _types, _args) => f(ExprRef::Fun(*fun)),
            Expr::Error
            | Expr::Num(_)
            | Expr::Bool(_)
            | Expr::Lam(_, _)
            | Expr::BinOp(_, _, _)
            | Expr::Let(_, _, _, _)
            | Expr::If(_, _, _)
            | Expr::Record(_)
            | Expr::Proj(_, _, _)
            | Expr::Variant(_, _, _)
            | Expr::Match(_, _) => {}
        }
        self.for_each_child(&mut |child| child.locatee.for_each_ref(f));
    }

    pub fn refs(&self) -> Vec<ExprRef> {
        let mut refs = Vec::new();
        self.for_each_ref(&mut |r| refs.push(r));
        refs
    }
}
