use crate::anf::*;
use mem::{Addr, Data, Memory, Tag};

pub mod mem;

type Stack<'a> = Vec<(ExprVar, Addr)>;

#[derive(Clone)]
enum Ctrl<'a> {
    Expr(&'a [Binding], &'a TailExpr),
    Value(Addr),
}

#[derive(Clone)]
enum Kont<'a> {
    Let(ExprVar, &'a [Binding], &'a TailExpr),
}

#[derive(Clone)]
pub struct Machine<'a> {
    ctrl: Ctrl<'a>,
    stack: Stack<'a>,
    memory: Memory<'a>,
    kont: Vec<(usize, Kont<'a>)>,
    funcs: im::HashMap<ExprVar, &'a FuncDecl>,
    args_tmp: Stack<'a>,
}

impl<'a> Machine<'a> {
    pub fn new(module: &'a Module, main: ExprVar) -> Self {
        let funcs: im::HashMap<_, _> =
            module.func_decls.iter().map(|decl| (decl.name, decl)).collect();
        let FuncDecl { name: _, params, body } = funcs.get(&main).unwrap();
        assert!(params.is_empty());
        Self {
            ctrl: Ctrl::from_expr(body),
            stack: Stack::new(),
            memory: Memory::new(),
            kont: Vec::new(),
            funcs,
            args_tmp: Vec::new(),
        }
    }

    pub fn run(mut self) -> (Addr, Memory<'a>) {
        let mut ctrl = std::mem::replace(&mut self.ctrl, Ctrl::Value(Addr::NULL));
        loop {
            // self.print_debug(&ctrl);
            ctrl = match ctrl {
                Ctrl::Expr(bindings, tail) => self.step_expr(bindings, tail),
                Ctrl::Value(addr) => {
                    if let Some(kont) = self.kont.pop() {
                        let (stack_level, Kont::Let(binder, bindings, tail)) = kont;
                        self.stack.truncate(stack_level);
                        self.push_stack(binder, addr);
                        Ctrl::Expr(bindings, tail)
                    } else {
                        // println!("Max stack: {}", self.stack.capacity());
                        return (addr, self.memory);
                    }
                }
            };
        }
    }

    /// Step when control contains an expression.
    fn step_expr(&mut self, bindings: &'a [Binding], tail: &'a TailExpr) -> Ctrl<'a> {
        let head = match bindings.split_first() {
            Some((Binding { binder, bindee }, bindings)) => {
                let stack_level = self.stack.len();
                self.kont.push((stack_level, Kont::Let(*binder, bindings, tail)));
                bindee
            }
            None => tail,
        };
        match head {
            Bindee::Error(span) => panic!("Bindee::Error({:?}) during execution", span),
            Bindee::Atom(atom) => Ctrl::Value(self.get_atom(atom)),
            Bindee::Num(n) => self.alloc_int(*n),
            Bindee::Bool(b) => self.alloc_bool(*b),
            Bindee::MakeClosure(closure) => self.step_make_closure(closure),
            Bindee::Record(_fields, values) => self.step_record(values),
            Bindee::Project(record, index, _field) => self.step_proj(record, *index),
            Bindee::Variant(rank, _constr, payload) => self.step_variant(*rank, payload),
            Bindee::BinOp(lhs, op, rhs) => self.step_binop(lhs, op, rhs),
            Bindee::AppClosure(clo, args) => self.step_app_closure(clo, args),
            Bindee::AppFunc(fun, args) => self.step_app_func(fun, args),
            Bindee::If(cond, then, elze) => self.step_if(cond, then, elze),
            Bindee::Match(scrut, branches) => self.step_match(scrut, branches),
        }
    }

    fn alloc_int(&mut self, n: i64) -> Ctrl<'a> {
        let addr = self.memory.alloc(Tag::Int, 0, 1);
        self.memory[addr + 1] = Data::from_int(n);
        Ctrl::Value(addr)
    }

    fn alloc_bool(&mut self, b: bool) -> Ctrl<'a> {
        Ctrl::Value(self.memory.alloc(Tag::Bool, b as u16, 0))
    }

    fn step_make_closure(&mut self, closure: &'a MakeClosure) -> Ctrl<'a> {
        let size = closure.captured.len() as u32 + 1;
        let addr = self.memory.alloc(Tag::Closure, 0, size);
        self.memory[addr + size] = Data::from_make_closure(closure);
        for (var, offset) in closure.captured.iter().zip(1..) {
            self.memory[addr + offset] = Data::from_addr(self.get_index(var));
        }
        Ctrl::Value(addr)
    }

    fn step_record(&mut self, values: &'a [Atom]) -> Ctrl<'a> {
        let addr = self.memory.alloc(Tag::Record, 0, values.len() as u32);
        for (value, offset) in values.iter().zip(1..) {
            self.memory[addr + offset] = Data::from_addr(self.get_atom(value));
        }
        Ctrl::Value(addr)
    }

    fn step_proj(&mut self, record: &'a Atom, index: u32) -> Ctrl<'a> {
        let addr = self.get_atom(record);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Record);
        debug_assert!(index < header.size);
        Ctrl::Value(self.memory[addr + index + 1].into_addr())
    }

    fn step_variant(&mut self, rank: u32, payload: &'a Option<Atom>) -> Ctrl<'a> {
        debug_assert!(rank <= u16::MAX as u32);
        let rank = rank as u16;
        let addr = match payload {
            None => self.memory.alloc(Tag::Variant, rank, 0),
            Some(payload) => {
                let addr = self.memory.alloc(Tag::Variant, rank, 1);
                self.memory[addr + 1] = Data::from_addr(self.get_atom(payload));
                addr
            }
        };
        Ctrl::Value(addr)
    }

    fn step_binop(&mut self, lhs: &'a Atom, op: &'a OpCode, rhs: &'a Atom) -> Ctrl<'a> {
        let lhs = self.get_int(lhs);
        let rhs = self.get_int(rhs);
        match op {
            OpCode::Add => self.alloc_int(lhs + rhs),
            OpCode::Sub => self.alloc_int(lhs - rhs),
            OpCode::Mul => self.alloc_int(lhs * rhs),
            OpCode::Div => self.alloc_int(lhs / rhs),
            OpCode::Equals => self.alloc_bool(lhs == rhs),
            OpCode::NotEq => self.alloc_bool(lhs != rhs),
            OpCode::Less => self.alloc_bool(lhs < rhs),
            OpCode::LessEq => self.alloc_bool(lhs <= rhs),
            OpCode::Greater => self.alloc_bool(lhs > rhs),
            OpCode::GreaterEq => self.alloc_bool(lhs >= rhs),
        }
    }

    fn step_app_closure(&mut self, clo: &'a Atom, args: &'a [Atom]) -> Ctrl<'a> {
        let addr = self.get_atom(clo);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Closure);
        let MakeClosure { captured, params, body } =
            self.memory[addr + header.size].into_make_closure();
        debug_assert_eq!(header.size as usize, captured.len() + 1);
        debug_assert_eq!(args.len(), params.len());
        for (param, arg) in params.iter().zip(args.iter()) {
            self.args_tmp.push((*param, self.get_atom(arg)));
        }
        self.stack.truncate(self.kont.last().map_or(0, |kont| kont.0));
        self.stack.reserve(captured.len() + params.len());
        for (var, offset) in captured.iter().zip(1..) {
            self.push_stack(var.1, self.memory[addr + offset].into_addr());
        }
        self.stack.append(&mut self.args_tmp);
        Ctrl::from_expr(body)
    }

    fn step_app_func(&mut self, fun: &'a ExprVar, args: &'a [Atom]) -> Ctrl<'a> {
        let FuncDecl { name: _, params, body } = self.funcs.get(fun).unwrap();
        assert_eq!(params.len(), args.len());
        for (param, arg) in params.iter().zip(args.iter()) {
            self.args_tmp.push((*param, self.get_atom(arg)));
        }
        self.stack.truncate(self.kont.last().map_or(0, |kont| kont.0));
        self.stack.append(&mut self.args_tmp);
        Ctrl::from_expr(body)
    }

    fn step_if(&self, cond: &'a Atom, then: &'a Expr, elze: &'a Expr) -> Ctrl<'a> {
        let addr = self.get_atom(cond);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Bool);
        Ctrl::from_expr(if header.rank != 0 { then } else { elze })
    }

    fn step_match(&mut self, scrut: &'a Atom, branches: &'a [Branch]) -> Ctrl<'a> {
        let addr = self.get_atom(scrut);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Variant);
        let Branch { pattern, rhs } = &branches[header.rank as usize];
        let Pattern { rank: _, constr: _, binder } = pattern;
        match binder {
            None => {
                debug_assert_eq!(header.size, 0);
            }
            Some(binder) => {
                debug_assert_eq!(header.size, 1);
                self.push_stack(*binder, self.memory[addr + 1].into_addr());
            }
        }
        Ctrl::from_expr(rhs)
    }

    fn push_stack(&mut self, binder: ExprVar, addr: Addr) {
        self.stack.push((binder, addr));
    }

    fn get_index(&self, index: &IdxVar) -> Addr {
        self.stack[self.stack.len() - index.0 as usize].1
    }

    fn get_atom(&self, atom: &Atom) -> Addr {
        self.get_index(&atom.0)
    }

    fn get_int(&self, atom: &Atom) -> i64 {
        let addr = self.get_atom(atom);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Int);
        self.memory[addr + 1].into_int()
    }

    #[allow(dead_code)]
    fn print_debug(&self, ctrl: &Ctrl<'a>) {
        let Self { ctrl: _, stack, memory, kont, funcs: _, args_tmp: _ } = self;
        print!("Control: ");
        match ctrl {
            Ctrl::Expr(bindings, tail) => {
                println!();
                for binding in *bindings {
                    println!("    {}", binding);
                }
                println!("    {}", tail);
            }
            Ctrl::Value(addr) => {
                println!("{}", memory.value_at(*addr));
            }
        }
        println!("{}", "-".repeat(60));
        println!("Environment:");
        for (binder, addr) in stack {
            println!("    {} = {}", binder, memory.value_at(*addr));
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
