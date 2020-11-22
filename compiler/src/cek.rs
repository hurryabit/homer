use crate::anf::*;
use mem::{Addr, Data, Memory, Tag};

pub mod mem;

type Stack<'a> = Vec<(ExprVar, Addr)>;

#[derive(Clone, Copy)]
struct Ctrl<'a> {
    bindings: &'a [Binding],
    stack_level: usize,
}

#[derive(Clone)]
struct Kont<'a> {
    binder: ExprVar,
    ctrl: Ctrl<'a>,
}

#[derive(Clone)]
pub struct Machine<'a> {
    ctrl: Ctrl<'a>,
    stack: Stack<'a>,
    memory: Memory<'a>,
    kont: Vec<Kont<'a>>,
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
            ctrl: Ctrl { bindings: &body.bindings, stack_level: 0 },
            stack: Stack::new(),
            memory: Memory::new(),
            kont: Vec::new(),
            funcs,
            args_tmp: Vec::new(),
        }
    }

    pub fn run(mut self) -> (Addr, Memory<'a>) {
        let mut ctrl = Ctrl { ..self.ctrl };
        loop {
            // self.print_debug(&ctrl);
            if let Some((binding, bindings)) = ctrl.bindings.split_first() {
                ctrl.bindings = bindings;
                self.step_expr(&mut ctrl, binding)
            } else if let Some(kont) = self.kont.pop() {
                let Kont { binder, ctrl: next_ctrl } = kont;
                self.stack[ctrl.stack_level] = (binder, self.stack.last().unwrap().1);
                self.stack.truncate(ctrl.stack_level + 1);
                ctrl = next_ctrl;
            } else {
                let result = self.stack.last().unwrap().1;
                return (result, self.memory);
            }
        }
    }

    /// Step when control contains an expression.
    fn step_expr(&mut self, ctrl: &mut Ctrl<'a>, binding: &'a Binding) {
        let Binding { binder, ref bindee } = *binding;
        match bindee {
            Bindee::Error(span) => panic!("Bindee::Error({:?}) during execution", span),
            Bindee::Atom(atom) => self.return_(binder, self.get_atom(atom)),
            Bindee::Num(n) => self.alloc_int(binder, *n),
            Bindee::Bool(b) => self.alloc_bool(binder, *b),
            Bindee::MakeClosure(closure) => self.alloc_closure(binder, closure),
            Bindee::Record(_fields, values) => self.alloc_record(binder, values),
            Bindee::Project(record, index, _field) => self.step_proj(binder, record, *index),
            Bindee::Variant(rank, _constr, payload) => self.alloc_variant(binder, *rank, payload),
            Bindee::BinOp(lhs, op, rhs) => self.step_binop(binder, &lhs, *op, &rhs),
            Bindee::AppClosure(clo, args) => self.step_app_closure(ctrl, binder, clo, args),
            Bindee::AppFunc(fun, args) => self.step_app_func(ctrl, binder, fun, args),
            Bindee::If(cond, then, elze) => self.step_if(ctrl, binder, cond, then, elze),
            Bindee::Match(scrut, branches) => self.step_match(ctrl, binder, scrut, branches),
        }
    }

    fn alloc_int(&mut self, binder: ExprVar, n: i64) {
        let addr = self.memory.alloc(Tag::Int, 0, 1);
        self.memory[addr + 1] = Data::from_int(n);
        self.return_(binder, addr);
    }

    fn alloc_bool(&mut self, binder: ExprVar, b: bool) {
        let addr = self.memory.alloc(Tag::Bool, b as u16, 0);
        self.return_(binder, addr);
    }

    fn alloc_closure(&mut self, binder: ExprVar, closure: &'a MakeClosure) {
        let size = closure.captured.len() as u32 + 1;
        let addr = self.memory.alloc(Tag::Closure, 0, size);
        self.memory[addr + size] = Data::from_make_closure(closure);
        for (var, offset) in closure.captured.iter().zip(1..) {
            self.memory[addr + offset] = Data::from_addr(self.get_index(var));
        }
        self.return_(binder, addr);
    }

    fn alloc_record(&mut self, binder: ExprVar, values: &'a [Atom]) {
        let addr = self.memory.alloc(Tag::Record, 0, values.len() as u32);
        for (value, offset) in values.iter().zip(1..) {
            self.memory[addr + offset] = Data::from_addr(self.get_atom(value));
        }
        self.return_(binder, addr);
    }

    fn step_proj(&mut self, binder: ExprVar, record: &'a Atom, index: u32) {
        let addr = self.get_atom(record);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Record);
        debug_assert!(index < header.size);
        self.return_(binder, self.memory[addr + index + 1].into_addr());
    }

    fn alloc_variant(&mut self, binder: ExprVar, rank: u32, payload: &'a Option<Atom>) {
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
        self.return_(binder, addr);
    }

    fn step_binop(&mut self, binder: ExprVar, lhs: &'a Atom, op: OpCode, rhs: &'a Atom) {
        let lhs = self.get_int(lhs);
        let rhs = self.get_int(rhs);
        match op {
            OpCode::Add => self.alloc_int(binder, lhs + rhs),
            OpCode::Sub => self.alloc_int(binder, lhs - rhs),
            OpCode::Mul => self.alloc_int(binder, lhs * rhs),
            OpCode::Div => self.alloc_int(binder, lhs / rhs),
            OpCode::Equals => self.alloc_bool(binder, lhs == rhs),
            OpCode::NotEq => self.alloc_bool(binder, lhs != rhs),
            OpCode::Less => self.alloc_bool(binder, lhs < rhs),
            OpCode::LessEq => self.alloc_bool(binder, lhs <= rhs),
            OpCode::Greater => self.alloc_bool(binder, lhs > rhs),
            OpCode::GreaterEq => self.alloc_bool(binder, lhs >= rhs),
        }
    }

    fn step_app_closure(
        &mut self,
        ctrl: &mut Ctrl<'a>,
        binder: ExprVar,
        clo: &'a Atom,
        args: &'a [Atom],
    ) {
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
        self.call_with_tco(ctrl, binder, body);
        self.stack.reserve(captured.len() + params.len());
        for (var, offset) in captured.iter().zip(1..) {
            self.stack.push((var.1, self.memory[addr + offset].into_addr()));
        }
        self.stack.append(&mut self.args_tmp);
    }

    fn step_app_func(
        &mut self,
        ctrl: &mut Ctrl<'a>,
        binder: ExprVar,
        fun: &'a ExprVar,
        args: &'a [Atom],
    ) {
        let FuncDecl { name: _, params, body } = self.funcs.get(fun).unwrap();
        assert_eq!(params.len(), args.len());
        for (param, arg) in params.iter().zip(args.iter()) {
            self.args_tmp.push((*param, self.get_atom(arg)));
        }
        self.call_with_tco(ctrl, binder, body);
        self.stack.append(&mut self.args_tmp);
    }

    fn step_if(
        &mut self,
        ctrl: &mut Ctrl<'a>,
        binder: ExprVar,
        cond: &'a Atom,
        then: &'a Expr,
        elze: &'a Expr,
    ) {
        let addr = self.get_atom(cond);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Bool);
        self.call(ctrl, binder, if header.rank != 0 { then } else { elze });
    }

    fn step_match(
        &mut self,
        ctrl: &mut Ctrl<'a>,
        binder: ExprVar,
        scrut: &'a Atom,
        branches: &'a [Branch],
    ) {
        let addr = self.get_atom(scrut);
        let header = self.memory[addr].into_header();
        debug_assert_eq!(header.tag, Tag::Variant);
        let Branch { pattern, rhs } = &branches[header.rank as usize];
        self.call(ctrl, binder, rhs);
        let Pattern { rank: _, constr: _, binder } = pattern;
        match binder {
            None => {
                debug_assert_eq!(header.size, 0);
            }
            Some(binder) => {
                debug_assert_eq!(header.size, 1);
                self.stack.push((*binder, self.memory[addr + 1].into_addr()));
            }
        }
    }

    fn return_(&mut self, binder: ExprVar, value: Addr) {
        self.stack.push((binder, value));
    }

    fn call(&mut self, ctrl: &mut Ctrl<'a>, binder: ExprVar, expr: &'a Expr) {
        let Expr { bindings } = expr;
        let stack_level = self.stack.len();
        let ctrl = std::mem::replace(ctrl, Ctrl { bindings, stack_level });
        self.kont.push(Kont { binder, ctrl });
    }

    fn call_with_tco(&mut self, ctrl: &mut Ctrl<'a>, binder: ExprVar, expr: &'a Expr) {
        if ctrl.bindings.is_empty() {
            let Expr { bindings } = expr;
            self.stack.truncate(ctrl.stack_level);
            ctrl.bindings = bindings;
        } else {
            self.call(ctrl, binder, expr);
        }
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
        println!("Control: (level = {})", ctrl.stack_level);
        for binding in ctrl.bindings {
            println!("    {}", binding);
        }
        println!("{}", "-".repeat(60));
        println!("Stack:");
        for (binder, addr) in stack.iter().rev() {
            println!("    {} = {}", binder, memory.value_at(*addr));
        }
        println!("{}", "-".repeat(60));
        println!("Kontinuations:");
        for Kont { binder, ctrl } in kont.iter().rev() {
            println!("  {}: (level = {})", binder, ctrl.stack_level);
            for binding in ctrl.bindings {
                println!("    {}", binding);
            }
        }
        println!("{}", "=".repeat(60));
    }
}
