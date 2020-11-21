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
                Self::App(fun, args) => {
                    yield_!(fun);
                    for arg in args {
                        yield_!(arg);
                    }
                }
                Self::BinOp(lhs, _opcode, rhs) => {
                    yield_!(lhs);
                    yield_!(rhs);
                }
                Self::FuncInst(fun, _types) => {
                    let _: &LExprVar = fun; // We want this to fail if we change the type of `fun`.
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
}
