use crate::anf::*;
use crate::util::in_parens_if_some;
use join_lazy_fmt::*;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

pub enum Value<'a> {
    Int(i64),
    Bool(bool),
    Record(&'a Vec<ExprVar>, Vec<RcValue<'a>>),
    Variant(u32, ExprCon, Option<RcValue<'a>>),
    Closure(Box<Closure<'a>>),
}

assert_eq_size!(Value, [usize; 5]);

pub type RcValue<'a> = Rc<Value<'a>>;

pub struct Closure<'a> {
    env: Stack<'a>,
    params: &'a [ExprVar],
    body: &'a Expr,
}

type Stack<'a> = Vec<(ExprVar, RcValue<'a>)>;

#[derive(Clone)]
enum Ctrl<'a> {
    Evaluating,
    Expr(&'a [Binding], &'a TailExpr),
    Value(RcValue<'a>),
}

#[derive(Clone)]
enum Kont<'a> {
    Let(ExprVar, &'a [Binding], &'a TailExpr),
}

#[derive(Clone)]
pub struct Machine<'a> {
    ctrl: Ctrl<'a>,
    stack: Stack<'a>,
    kont: Vec<(usize, Kont<'a>)>,
    funcs: im::HashMap<ExprVar, &'a FuncDecl>,
    args_tmp: Stack<'a>,
}

impl<'a> Machine<'a> {
    pub fn new(module: &'a Module, main: ExprVar) -> Self {
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
        Self {
            ctrl: Ctrl::from_expr(body),
            stack: Stack::new(),
            kont: Vec::new(),
            funcs,
            args_tmp: Vec::new(),
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
                        let (stack_level, Kont::Let(binder, bindings, tail)) = kont;
                        self.stack.truncate(stack_level);
                        self.push_stack(binder, value);
                        Ctrl::Expr(bindings, tail)
                    } else {
                        // println!("Max stack: {}", self.stack.capacity());
                        return value;
                    }
                }
            };
            self.ctrl = new_ctrl;
        }
    }

    /// Step when control contains an expression.
    fn step_expr(&mut self, bindings: &'a [Binding], tail: &'a TailExpr) -> Ctrl<'a> {
        let head = match bindings.split_first() {
            Some((Binding { binder, bindee }, bindings)) => {
                let stack_level = self.stack.len();
                self.kont
                    .push((stack_level, Kont::Let(*binder, bindings, tail)));
                bindee
            }
            None => tail,
        };
        match head {
            Bindee::Error(span) => panic!("Bindee::Error({:?}) during execution", span),
            Bindee::Atom(atom) => Ctrl::Value(Rc::clone(self.get_atom(atom))),
            Bindee::Num(n) => Ctrl::Value(Rc::new(Value::Int(*n))),
            Bindee::Bool(b) => Ctrl::Value(Rc::new(Value::Bool(*b))),
            Bindee::MakeClosure(closure) => self.step_make_closure(closure),
            Bindee::Record(fields, values) => self.step_record(fields, values),
            Bindee::Project(record, index, _field) => self.step_proj(record, *index),
            Bindee::Variant(rank, constr, payload) => self.step_variant(*rank, constr, payload),
            Bindee::BinOp(lhs, op, rhs) => {
                Ctrl::Value(op.execute(self.get_atom(lhs), self.get_atom(rhs)))
            }
            Bindee::AppClosure(clo, args) => self.step_app_closure(clo, args),
            Bindee::AppFunc(fun, args) => self.step_app_func(fun, args),
            Bindee::If(cond, then, elze) => self.step_if(cond, then, elze),
            Bindee::Match(scrut, branches) => self.step_match(scrut, branches),
        }
    }

    fn step_make_closure(&self, closure: &'a MakeClosure) -> Ctrl<'a> {
        let MakeClosure {
            captured,
            params,
            body,
        } = closure;
        let env = captured
            .iter()
            .map(|index| (index.1, Rc::clone(self.get_index(index))))
            .collect();
        Ctrl::Value(Rc::new(Value::Closure(Box::new(Closure {
            env,
            params,
            body,
        }))))
    }

    #[allow(clippy::ptr_arg)]
    fn step_record(&self, fields: &'a Vec<ExprVar>, values: &'a [Atom]) -> Ctrl<'a> {
        let values = values
            .iter()
            .map(|atom| Rc::clone(self.get_atom(atom)))
            .collect();
        Ctrl::Value(Rc::new(Value::Record(fields, values)))
    }

    fn step_proj(&self, record: &'a Atom, index: u32) -> Ctrl<'a> {
        if let Value::Record(_fields, values) = self.get_atom(record).as_ref() {
            Ctrl::Value(Rc::clone(&values[index as usize]))
        } else {
            panic!("Projection on non-record: {}", record)
        }
    }

    fn step_variant(&self, rank: u32, constr: &'a ExprCon, payload: &'a Option<Atom>) -> Ctrl<'a> {
        let payload = payload
            .as_ref()
            .map(|payload| Rc::clone(self.get_atom(payload)));
        Ctrl::Value(Rc::new(Value::Variant(rank, *constr, payload)))
    }

    fn step_app_closure(&mut self, clo: &'a Atom, args: &'a [Atom]) -> Ctrl<'a> {
        let closure = Rc::clone(self.get_atom(clo));
        if let Value::Closure(closure) = closure.as_ref() {
            let Closure { env, params, body } = closure.as_ref();
            assert_eq!(params.len(), args.len());
            for (param, arg) in params.iter().zip(args.iter()) {
                self.args_tmp.push((*param, Rc::clone(self.get_atom(arg))));
            }
            self.stack
                .truncate(self.kont.last().map_or(0, |kont| kont.0));
            self.stack.extend_from_slice(env);
            self.stack.append(&mut self.args_tmp);
            Ctrl::from_expr(body)
        } else {
            panic!("Application on non-closure")
        }
    }

    fn step_app_func(&mut self, fun: &'a ExprVar, args: &'a [Atom]) -> Ctrl<'a> {
        let FuncDecl {
            name: _,
            params,
            body,
        } = self.funcs.get(fun).unwrap();
        assert_eq!(params.len(), args.len());
        for (param, arg) in params.iter().zip(args.iter()) {
            self.args_tmp.push((*param, Rc::clone(self.get_atom(arg))));
        }
        self.stack
            .truncate(self.kont.last().map_or(0, |kont| kont.0));
        self.stack.append(&mut self.args_tmp);
        Ctrl::from_expr(body)
    }

    fn step_if(&self, cond: &'a Atom, then: &'a Expr, elze: &'a Expr) -> Ctrl<'a> {
        if let Value::Bool(cond) = self.get_atom(cond).as_ref() {
            Ctrl::from_expr(if *cond { then } else { elze })
        } else {
            panic!("If on non-bool")
        }
    }

    fn step_match(&mut self, scrut: &'a Atom, branches: &'a [Branch]) -> Ctrl<'a> {
        if let Value::Variant(rank, constr, payload) = self.get_atom(scrut).as_ref() {
            if let Some(Branch { pattern, rhs }) =
                branches.iter().find(|branch| branch.pattern.rank == *rank)
            {
                let Pattern {
                    rank: _,
                    constr: _,
                    binder,
                } = pattern;
                match (binder.as_ref(), payload.as_ref().cloned()) {
                    (None, Some(_)) | (Some(_), None) => panic!("Pattern/payload mismatch"),
                    (None, None) => (),
                    (Some(binder), Some(payload)) => {
                        self.push_stack(*binder, payload);
                    }
                };
                Ctrl::from_expr(rhs)
            } else {
                panic!("Unmatched constructor: {:?}", constr)
            }
        } else {
            panic!("Match on non-variant")
        }
    }

    fn push_stack(&mut self, binder: ExprVar, value: RcValue<'a>) {
        self.stack.push((binder, value));
    }

    fn get_index(&self, index: &IdxVar) -> &RcValue<'a> {
        &self.stack[self.stack.len() - index.0 as usize].1
    }

    fn get_atom(&self, atom: &Atom) -> &RcValue<'a> {
        self.get_index(&atom.0)
    }

    #[allow(dead_code)]
    fn print_debug(&self) {
        let Self {
            ctrl,
            stack,
            kont,
            funcs: _,
            args_tmp: _,
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
        for (binder, value) in stack {
            println!("    {} = {}", binder, value);
        }
        println!("{}", "-".repeat(60));
        println!("Kontinuations:");
        for (_, Kont::Let(binder, _, _)) in kont.iter().rev() {
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
    fn execute<'a>(self, lhs: &RcValue<'a>, rhs: &RcValue<'a>) -> RcValue<'a> {
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
        Rc::new(value)
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
            | (Int(_), Record(..))
            | (Int(_), Variant(..))
            | (Int(_), Closure(..)) => Some(Ordering::Less),
            (Bool(_), Int(_)) => Some(Ordering::Greater),
            (Bool(b1), Bool(b2)) => Some(b1.cmp(b2)),
            (Bool(_), Record(..)) | (Bool(_), Variant(..)) | (Bool(_), Closure(..)) => {
                Some(Ordering::Less)
            }
            (Record(..), Int(_)) | (Record(..), Bool(_)) => Some(Ordering::Greater),
            (Record(_, values1), Record(_, values2)) => values1.partial_cmp(values2),
            (Record(..), Variant(..)) | (Record(..), Closure(..)) => Some(Ordering::Less),
            (Variant(..), Int(_)) | (Variant(..), Bool(_)) | (Variant(..), Record(..)) => {
                Some(Ordering::Greater)
            }
            (Variant(rank1, _constr1, payload1), Variant(rank2, _constr2, payload2)) => {
                match rank1.cmp(rank2) {
                    Ordering::Less => Some(Ordering::Less),
                    Ordering::Greater => Some(Ordering::Greater),
                    Ordering::Equal => payload1.partial_cmp(payload2),
                }
            }
            (Variant(..), Closure(..)) => Some(Ordering::Less),
            (Closure(..), Int(_))
            | (Closure(..), Bool(_))
            | (Closure(..), Record(..))
            | (Closure(..), Variant(..)) => Some(Ordering::Greater),
            (Closure(..), Closure(..)) => None,
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Record(fields, values) => write!(
                f,
                "{{{}}}",
                ", ".join(
                    fields
                        .iter()
                        .zip(values.iter())
                        .map(|(field, value)| lazy_format!("{} = {}", field, value))
                )
            ),
            Value::Variant(rank, constr, value) => {
                write!(f, "{}/{}{}", constr, rank, in_parens_if_some(value))
            }
            Value::Closure(closure) => {
                let Closure {
                    env,
                    params,
                    body: _,
                } = closure.as_ref();
                write!(
                    f,
                    "[{env}; {params}; ...]",
                    env = ", ".join(
                        env.iter()
                            .map(|(binder, value)| lazy_format!("{} = {}", binder, value))
                    ),
                    params = ", ".join(*params),
                )
            }
        }
    }
}

impl<'a> Default for Ctrl<'a> {
    fn default() -> Self {
        Self::Evaluating
    }
}
