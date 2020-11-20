use crate::anf::*;
use crate::util::in_parens_if_some;
use join_lazy_fmt::*;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

pub enum Value<'a> {
    Int(i64),
    Bool(bool),
    Record(Vec<(ExprVar, RcValue<'a>)>),
    Variant(ExprCon, Option<RcValue<'a>>),
    Closure(Env<'a>, &'a [ExprVar], &'a Expr),
}

pub type RcValue<'a> = Rc<Value<'a>>;

type Env<'a> = im::HashMap<ExprVar, RcValue<'a>>;

enum Ctrl<'a> {
    Evaluating,
    Expr(&'a [Binding], &'a TailExpr),
    Value(RcValue<'a>),
}

enum Kont<'a> {
    Let(Env<'a>, ExprVar, &'a [Binding], &'a TailExpr),
}

pub struct Machine<'a> {
    ctrl: Ctrl<'a>,
    env: Env<'a>,
    kont: Vec<Kont<'a>>,
    funcs: im::HashMap<ExprVar, &'a FuncDecl>,
}

impl<'a> Machine<'a> {
    pub fn new(module: &'a Module, main: ExprVar) -> Self {
        let env = Env::new();
        let kont = Vec::new();
        let funcs: im::HashMap<_, _> = module
            .func_decls
            .iter()
            .map(|decl| (decl.name, decl))
            .collect();
        let FuncDecl {
            name: _,
            params,
            body,
        } = funcs.get(&main).unwrap();
        assert!(params.is_empty());
        let ctrl = Ctrl::from_expr(body);
        Self {
            ctrl,
            env,
            kont,
            funcs,
        }
    }

    pub fn run(mut self) -> RcValue<'a> {
        loop {
            // self.print_debug();
            let old_ctrl = std::mem::take(&mut self.ctrl);
            let new_ctrl = match old_ctrl {
                Ctrl::Evaluating => panic!("Machine control has not been set after step"),
                Ctrl::Expr(bindings, tail) => self.step_expr(bindings, tail),
                Ctrl::Value(value) => {
                    if let Some(kont) = self.kont.pop() {
                        let Kont::Let(env, binder, bindings, tail) = kont;
                        self.env = env.update(binder, value);
                        Ctrl::Expr(bindings, tail)
                    } else {
                        return value;
                    }
                }
            };
            self.ctrl = new_ctrl;
        }
    }

    /// Step when control contains an expression.
    fn step_expr(&mut self, bindings: &'a [Binding], tail: &'a TailExpr) -> Ctrl<'a> {
        let (head, mut rest) = if let Some((Binding { binder, bindee }, bindings)) = bindings.split_first() {
            (bindee, Some((binder, bindings, tail)))
        } else {
            (tail, None)
        };
        let ctrl = match head {
            Bindee::Error(span) => panic!("Bindee::Error({:?}) during execution", span),
            Bindee::Atom(atom) => Ctrl::Value(Rc::clone(self.get_atom(atom))),
            Bindee::Num(n) => Ctrl::Value(Rc::new(Value::Int(*n))),
            Bindee::Bool(b) => Ctrl::Value(Rc::new(Value::Bool(*b))),
            Bindee::MakeClosure(closure) => self.step_make_closure(closure),
            Bindee::Record(fields) => self.step_record(fields),
            Bindee::Project(record, field) => self.step_proj(record, field),
            Bindee::Variant(constr, payload) => self.step_variant(constr, payload),
            Bindee::BinOp(lhs, op, rhs) => op.execute(self.get_atom(lhs), self.get_atom(rhs)),
            Bindee::AppClosure(clo, args) => {
                let (mut env, ctrl) = self.step_app_closure(clo, args);
                if let Some((binder, bindings, tail)) = rest.take() {
                    std::mem::swap(&mut self.env, &mut env);
                    self.kont.push(Kont::Let(env, *binder, bindings, tail));
                } else {
                    self.env = env;
                }
                ctrl
            }
            Bindee::AppFunc(fun, args) => {
                let (mut env, ctrl) = self.step_app_func(fun, args);
                if let Some((binder, bindings, tail)) = rest.take() {
                    std::mem::swap(&mut self.env, &mut env);
                    self.kont.push(Kont::Let(env, *binder, bindings, tail));
                } else {
                    self.env = env;
                }
                ctrl
            }
            Bindee::If(cond, then, elze) => {
                if let Some((binder, bindings, tail)) = rest.take() {
                    self.kont
                        .push(Kont::Let(self.env.clone(), *binder, bindings, tail));
                }
                self.step_if(cond, then, elze)
            }
            Bindee::Match(scrut, branches) => {
                if let Some((binder, bindings, tail)) = rest.take() {
                    self.kont
                        .push(Kont::Let(self.env.clone(), *binder, bindings, tail));
                }
                self.step_match(scrut, branches)
            }
        };
        if let Some((binder, bindings, tail)) = rest.take() {
            if let Ctrl::Value(value) = ctrl {
                self.env.insert(*binder, value);
                Ctrl::Expr(bindings, tail)
            } else {
                panic!("IMPOSSIBLE: Unhandled rest and value at the same time")
            }
        } else {
            ctrl
        }
    }

    fn step_make_closure(&self, closure: &'a MakeClosure) -> Ctrl<'a> {
        let MakeClosure {
            captured,
            params,
            body,
        } = closure;
        let closure_env = captured
            .iter()
            .map(|var| (*var, Rc::clone(self.env.get(var).unwrap())))
            .collect();
        Ctrl::Value(Rc::new(Value::Closure(closure_env, params, body)))
    }

    fn step_record(&self, fields: &'a [(ExprVar, Atom)]) -> Ctrl<'a> {
        let fields = fields
            .iter()
            .map(|(field, atom)| (*field, Rc::clone(self.get_atom(atom))))
            .collect();
        Ctrl::Value(Rc::new(Value::Record(fields)))
    }

    fn step_proj(&self, record: &'a Atom, field: &'a ExprVar) -> Ctrl<'a> {
        if let Value::Record(fields) = self.get_atom(record).as_ref() {
            if let Ok(index) = fields.binary_search_by_key(field, |entry| entry.0) {
                Ctrl::Value(Rc::clone(&fields[index].1))
            } else {
                panic!("Projection on unknown field")
            }
        } else {
            panic!("Projection on non-record: {}", record)
        }
    }

    fn step_variant(&self, constr: &'a ExprCon, payload: &'a Option<Atom>) -> Ctrl<'a> {
        let payload = payload
            .as_ref()
            .map(|payload| Rc::clone(self.get_atom(payload)));
        Ctrl::Value(Rc::new(Value::Variant(*constr, payload)))
    }

    fn step_app_closure(&self, clo: &'a Atom, args: &'a [Atom]) -> (Env<'a>, Ctrl<'a>) {
        let closure = self.get_atom(clo);
        if let Value::Closure(captured, params, body) = closure.as_ref() {
            let body = *body;
            assert_eq!(params.len(), args.len());
            let mut env = captured.clone();
            env.extend(
                params
                    .iter()
                    .zip(args.iter())
                    .map(|(param, arg)| (*param, Rc::clone(self.get_atom(arg)))),
            );
            (env, Ctrl::from_expr(body))
        } else {
            panic!("Application on non-closure")
        }
    }

    fn step_app_func(&self, fun: &'a ExprVar, args: &'a [Atom]) -> (Env<'a>, Ctrl<'a>) {
        let FuncDecl {
            name: _,
            params,
            body,
        } = self.funcs.get(fun).unwrap();
        assert_eq!(params.len(), args.len());
        let env = params
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| (*param, Rc::clone(self.get_atom(arg))))
            .collect();
        (env, Ctrl::from_expr(body))
    }

    fn step_if(&self, cond: &'a Atom, then: &'a Expr, elze: &'a Expr) -> Ctrl<'a> {
        if let Value::Bool(b) = self.get_atom(cond).as_ref() {
            Ctrl::from_expr(if *b { then } else { elze })
        } else {
            panic!("If on non-bool")
        }
    }

    fn step_match(&mut self, scrut: &'a Atom, branches: &'a [Branch]) -> Ctrl<'a> {
        if let Value::Variant(constr, payload) = self.get_atom(scrut).as_ref() {
            let payload = payload.as_ref().cloned();
            if let Some(Branch { pattern, rhs: expr }) = branches
                .iter()
                .find(|branch| branch.pattern.constr == *constr)
            {
                let Pattern {
                    constr: _,
                    binder: pat_binder,
                } = pattern;
                match (pat_binder.as_ref(), payload) {
                    (None, Some(_)) | (Some(_), None) => panic!("Pattern/payload mismatch"),
                    (None, None) => (),
                    (Some(pat_binder), Some(payload)) => {
                        self.env.insert(*pat_binder, payload);
                    }
                };
                Ctrl::from_expr(expr)
            } else {
                panic!("Unmatched constructor: {:?}", constr)
            }
        } else {
            panic!("Match on non-variant")
        }
    }

    fn get_atom(&self, atom: &Atom) -> &RcValue<'a> {
        self.env.get(&atom.0).unwrap()
    }

    #[allow(dead_code)]
    fn print_debug(&self) {
        let Self {
            ctrl,
            env,
            kont,
            funcs: _,
        } = self;
        print!("Control: ");
        match ctrl {
            Ctrl::Evaluating => println!("???"),
            Ctrl::Expr(bindings, tail) => {
                println!();
                for binding in *bindings {
                    println!("    {}", binding);
                }
                println!("    {}", tail);
            }
            Ctrl::Value(value) => {
                println!("{}", value);
            }
        }
        println!("{}", "-".repeat(60));
        println!("Environment:");
        for (binder, value) in env {
            println!("    {} = {}", binder, value);
        }
        println!("{}", "-".repeat(60));
        println!("Kontinuations:");
        for Kont::Let(_, binder, _, _) in kont.iter().rev() {
            println!("    {}", binder);
        }
        println!("{}", "=".repeat(60));
    }
}

impl<'a> Ctrl<'a> {
    fn from_expr(expr: &'a Expr) -> Self {
        Self::Expr(&expr.bindings, &expr.tail)
    }
}

impl OpCode {
    fn execute<'a>(self, lhs: &RcValue<'a>, rhs: &RcValue<'a>) -> Ctrl<'a> {
        let value = match self {
            OpCode::Add => Value::Int(lhs.as_i64() + rhs.as_i64()),
            OpCode::Sub => Value::Int(lhs.as_i64() - rhs.as_i64()),
            OpCode::Mul => Value::Int(lhs.as_i64() * rhs.as_i64()),
            OpCode::Div => Value::Int(lhs.as_i64() / rhs.as_i64()),
            OpCode::Equals => Value::Bool(lhs == rhs),
            OpCode::NotEq => Value::Bool(lhs != rhs),
            OpCode::Less => Value::Bool(lhs < rhs),
            OpCode::LessEq => Value::Bool(lhs <= rhs),
            OpCode::Greater => Value::Bool(lhs > rhs),
            OpCode::GreaterEq => Value::Bool(lhs >= rhs),
        };
        Ctrl::Value(Rc::new(value))
    }
}

impl<'a> Value<'a> {
    fn as_i64(&self) -> i64 {
        if let Value::Int(n) = self {
            *n
        } else {
            panic!("Expected Int, found something else")
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Equal))
    }
}

impl<'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;
        match (self, other) {
            (Int(n1), Int(n2)) => Some(n1.cmp(n2)),
            (Int(_), Bool(_))
            | (Int(_), Record(_))
            | (Int(_), Variant(..))
            | (Int(_), Closure(..)) => Some(Ordering::Less),
            (Bool(_), Int(_)) => Some(Ordering::Greater),
            (Bool(b1), Bool(b2)) => Some(b1.cmp(b2)),
            (Bool(_), Record(_)) | (Bool(_), Variant(..)) | (Bool(_), Closure(..)) => {
                Some(Ordering::Less)
            }
            (Record(_), Int(_)) | (Record(_), Bool(_)) => Some(Ordering::Greater),
            (Record(fs1), Record(fs2)) => {
                let fields1 = fs1.iter().map(|f| f.0);
                let fields2 = fs2.iter().map(|f| f.0);
                match fields1.cmp(fields2) {
                    Ordering::Less => Some(Ordering::Less),
                    Ordering::Greater => Some(Ordering::Greater),
                    Ordering::Equal => {
                        let values1 = fs1.iter().map(|f| Rc::clone(&f.1));
                        let values2 = fs2.iter().map(|f| Rc::clone(&f.1));
                        values1.partial_cmp(values2)
                    }
                }
            }
            (Record(_), Variant(..)) | (Record(_), Closure(..)) => Some(Ordering::Less),
            (Variant(..), Int(_)) | (Variant(..), Bool(_)) | (Variant(..), Record(_)) => {
                Some(Ordering::Greater)
            }
            (Variant(c1, v1), Variant(c2, v2)) => match c1.cmp(c2) {
                Ordering::Less => Some(Ordering::Less),
                Ordering::Greater => Some(Ordering::Greater),
                Ordering::Equal => v1.partial_cmp(v2),
            },
            (Variant(..), Closure(..)) => Some(Ordering::Less),
            (Closure(..), Int(_))
            | (Closure(..), Bool(_))
            | (Closure(..), Record(_))
            | (Closure(..), Variant(..)) => Some(Ordering::Greater),
            (Closure(..), Closure(..)) => None,
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Int(n) => write!(f, "{}", n),
            Bool(b) => write!(f, "{}", b),
            Record(fields) => write!(
                f,
                "{{{}}}",
                ", ".join(
                    fields
                        .iter()
                        .map(|(field, value)| lazy_format!("{} = {}", field, value))
                )
            ),
            Variant(constr, value) => write!(f, "{}{}", constr, in_parens_if_some(value)),
            Closure(captured, params, _body) => write!(
                f,
                "[{captured}; {params}; ...]",
                captured = ", ".join(
                    captured
                        .iter()
                        .map(|(binder, value)| lazy_format!("{} = {}", binder, value))
                ),
                params = ", ".join(*params),
            ),
        }
    }
}

impl<'a> Default for Ctrl<'a> {
    fn default() -> Self {
        Self::Evaluating
    }
}
