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
        use Type::*;
        gen!({
            match self {
                Error => {}
                Var(_) | Int | Bool => {}
                SynApp(syn, args) => {
                    let _: &LTypeVar = syn; // We want this to break if change the type of `syn`.
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
}

impl LExpr {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LExpr> {
        self.locatee.children_mut()
    }
}

impl Expr {
    pub fn children_mut(&mut self) -> impl Iterator<Item = &mut LExpr> {
        use genawaiter::{rc::gen, yield_};
        use Expr::*;
        gen!({
            match self {
                Error => {}
                Var(_) | Num(_) | Bool(_) => {}
                Lam(_params, body) => {
                    yield_!(body.as_mut());
                }
                App(fun, args) => {
                    yield_!(fun);
                    for arg in args {
                        yield_!(arg);
                    }
                }
                BinOp(lhs, _opcode, rhs) => {
                    yield_!(lhs);
                    yield_!(rhs);
                }
                FuncInst(fun, _types) => {
                    let _: &LExprVar = fun; // We want this to fail if we change the type of `fun`.
                }
                Let(_binder, _type, bindee, body) => {
                    yield_!(bindee);
                    yield_!(body);
                }
                If(cond, then, elze) => {
                    yield_!(cond);
                    yield_!(then);
                    yield_!(elze);
                }
                Record(fields) => {
                    for (_name, expr) in fields {
                        yield_!(expr);
                    }
                }
                Proj(record, _field) => {
                    yield_!(record);
                }
                Variant(_constr, opt_payload) => {
                    if let Some(payload) = opt_payload {
                        yield_!(payload);
                    }
                }
                Match(scrut, branches) => {
                    yield_!(scrut);
                    for branch in branches {
                        yield_!(&mut branch.body);
                    }
                }
            }
        })
        .into_iter()
    }
}
