use super::*;

#[derive(Clone, Default)]
struct Env(im::HashMap<ExprVar, usize>);

impl Env {
    fn intro_binder(mut self, binder: ExprVar) -> Self {
        let len = self.0.len();
        self.0.insert(binder, len);
        self
    }

    fn index(&self, binder: ExprVar) -> IdxVar {
        let index = self.0.len() - self.0.get(&binder).unwrap();
        IdxVar(index as u32, binder)
    }
}

impl FuncDecl {
    pub fn index(&mut self) {
        let Self {
            name: _,
            params,
            body,
        } = self;
        let env = params.iter().fold(Env::default(), |env, param| env.intro_binder(*param));
        body.index(&env);
    }
}

impl Expr {
    fn index(&mut self, env: &Env) {
        let Self { bindings, tail } = self;
        let env = bindings
            .iter_mut()
            .fold(env.clone(), |env, Binding { binder, bindee }| {
                bindee.index(&env);
                env.intro_binder(*binder)
            });
        tail.index(&env);
    }
}

impl Bindee {
    fn index(&mut self, env: &Env) {
        use Bindee::*;
        match self {
            Error(_) => {}
            Atom(var) => {
                var.index(env);
            }
            Num(_) | Bool(_) => {}
            MakeClosure(closure) => {
                closure.index(env);
            }
            AppClosure(clo, args) => {
                clo.index(env);
                for arg in args {
                    arg.index(env);
                }
            }
            AppFunc(_fun, args) => {
                for arg in args {
                    arg.index(env);
                }
            }
            BinOp(lhs, _op, rhs) => {
                lhs.index(env);
                rhs.index(env);
            }
            If(cond, then, elze) => {
                cond.index(env);
                then.index(env);
                elze.index(env);
            }
            Record(_fields, values) => {
                for value in values {
                    value.index(env);
                }
            }
            Project(record, _index, _field) => {
                record.index(env);
            }
            Variant(_rank, _constr, payload) => {
                if let Some(payload) = payload {
                    payload.index(env);
                }
            }
            Match(scrut, branches) => {
                scrut.index(env);
                for branch in branches {
                    branch.index(env);
                }
            }
        }
    }
}

impl IdxVar {
    fn index(&mut self, env: &Env) {
        *self = env.index(self.1);
    }
}

impl Atom {
    fn index(&mut self, env: &Env) {
        self.0.index(env);
    }
}

impl Branch {
    fn index(&mut self, env: &Env) {
        let Self { pattern, rhs } = self;
        let Pattern { rank: _, constr: _, binder } = pattern;
        if let Some(binder) = binder {
            rhs.index(&env.clone().intro_binder(*binder))
        } else {
            rhs.index(env)
        };
    }
}

impl MakeClosure {
    fn index(&mut self, env: &Env) {
        let Self {
            captured,
            params,
            body,
        } = self;
        for var in captured.iter_mut() {
            var.index(env);
        }
        let env = captured
            .iter()
            .fold(Env::default(), |env, var| env.intro_binder(var.1));
        let env = params
            .iter()
            .fold(env, |env, param| env.intro_binder(*param));
        body.index(&env);
    }
}
