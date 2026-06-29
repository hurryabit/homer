use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use wasm_encoder as wasm;

use super::type_idx;
use crate::syntax::Expr;
use crate::syntax::ExprVar;
use crate::syntax::FuncDecl;
use crate::syntax::LExpr;
use crate::syntax::LExprVar;
use crate::syntax::LType;
use crate::syntax::Module;
use crate::syntax::OpCode;

pub(crate) enum QueueItem<'m> {
    FuncDecl(&'m FuncDecl),
    ClosureFunc { captures: Vec<ExprVar>, params: &'m [(LExprVar, Option<LType>)], body: &'m LExpr },
}

pub(crate) struct FuncManager<'m> {
    module: &'m Module,
    func_indices: HashMap<ExprVar, u32>,
    completed: u32,
    queue: VecDeque<QueueItem<'m>>,
    ref_func_targets: HashSet<u32>,
}

impl<'m> FuncManager<'m> {
    pub(crate) fn new(module: &'m Module) -> Self {
        let queue: VecDeque<_> = module.func_decls().map(QueueItem::FuncDecl).collect();
        let func_indices = module
            .func_decls()
            .enumerate()
            .map(|(idx, func)| (func.name.locatee, idx as u32))
            .collect();
        Self { module, func_indices, completed: 0, queue, ref_func_targets: HashSet::new() }
    }

    pub(crate) fn finish(self) -> HashSet<u32> {
        self.ref_func_targets
    }

    fn func_index(&self, name: ExprVar) -> u32 {
        self.func_indices[&name]
    }

    fn push_closure(
        &mut self,
        captures: Vec<ExprVar>,
        params: &'m [(LExprVar, Option<LType>)],
        body: &'m LExpr,
    ) -> u32 {
        let index = self.completed + self.queue.len() as u32;
        self.queue.push_back(QueueItem::ClosureFunc { captures, params, body });
        index
    }

    pub(crate) fn compile_next(&mut self) -> Option<(wasm::Function, u32)> {
        let item = self.queue.pop_front()?;
        self.completed += 1;
        let res = match item {
            QueueItem::FuncDecl(func) => self.compile_func_decl(func),
            QueueItem::ClosureFunc { captures, params, body } => {
                self.compile_closure_func(captures, params, body)
            }
        };
        Some(res)
    }

    fn compile_func_decl(&mut self, func: &'m FuncDecl) -> (wasm::Function, u32) {
        let arity = func.expr_params.len();
        let local_vars: Vec<_> = func.expr_params.iter().map(|(name, _)| name.locatee).collect();
        let mut compiler = ExprCompiler::new(self, local_vars);
        compiler.compile_expr(&func.body.locatee);
        let (bytes, max_local_vars_len) = compiler.finish();
        let extra_locals = (max_local_vars_len - arity) as u32;
        let mut func = wasm::Function::new([(extra_locals, super::ref_type(type_idx::VALUE))]);
        func.raw(bytes);
        (func, type_idx::FUNC[arity])
    }

    fn compile_closure_func(
        &mut self,
        captures: Vec<ExprVar>,
        params: &'m [(LExprVar, Option<LType>)],
        body: &'m LExpr,
    ) -> (wasm::Function, u32) {
        // TODO: Find something better than eagerly putting all captured values into locals.
        let arity = params.len();
        let local_vars: Vec<_> = std::iter::once(ExprVar::new("$captures"))
            .chain(params.iter().map(|(name, _)| name.locatee))
            .chain(captures.iter().copied())
            .collect();
        let mut compiler = ExprCompiler::new(self, local_vars);
        let captures_offset = arity as u32 + 1;
        for index in 0..captures.len() as u32 {
            compiler
                .instructions()
                .local_get(0)
                .i32_const(index as i32)
                .array_get(type_idx::VALUE_ARRAY)
                .local_set(captures_offset + index);
        }
        compiler.compile_expr(&body.locatee);
        let (bytes, max_local_vars_len) = compiler.finish();
        let extra_locals = (max_local_vars_len - arity - 1) as u32;
        let mut func = wasm::Function::new([(extra_locals, super::ref_type(type_idx::VALUE))]);
        func.raw(bytes);
        (func, type_idx::CLOSURE_FUNC[arity])
    }
}

struct ExprCompiler<'a, 'm> {
    manager: &'a mut FuncManager<'m>,
    local_vars: Vec<ExprVar>,
    max_local_vars_len: usize,
    var_indices: HashMap<ExprVar, u32>,
    bytes: Vec<u8>,
}

impl<'a, 'm> ExprCompiler<'a, 'm> {
    pub(crate) fn new(manager: &'a mut FuncManager<'m>, local_vars: Vec<ExprVar>) -> Self {
        let max_num_local_vars = local_vars.len();
        let var_indices =
            local_vars.iter().enumerate().map(|(index, var)| (*var, index as u32)).collect();
        let bytes = Vec::new();
        Self { manager, local_vars, max_local_vars_len: max_num_local_vars, var_indices, bytes }
    }

    pub(crate) fn finish(mut self) -> (Vec<u8>, usize) {
        self.instructions().end();
        (self.bytes, self.max_local_vars_len)
    }

    fn instructions(&mut self) -> wasm::InstructionSink<'_> {
        wasm::InstructionSink::new(&mut self.bytes)
    }

    fn intro_local(&mut self, var: ExprVar, f: impl FnOnce(&mut Self, u32)) {
        let index = self.local_vars.len() as u32;
        let old_index = self.var_indices.insert(var, index);
        self.local_vars.push(var);
        self.max_local_vars_len = self.max_local_vars_len.max(self.local_vars.len());
        f(self, index);
        self.local_vars.pop();
        if let Some(old_index) = old_index {
            self.var_indices.insert(var, old_index);
        } else {
            self.var_indices.remove(&var);
        }
    }

    fn compile_expr(&mut self, expr: &'m Expr) {
        match expr {
            Expr::Error => {
                // TODO: Error compilation and hide the current behaviour behind a flag.
                self.instructions().unreachable();
            }
            Expr::Var(var) => {
                let index = self.var_indices[&var.locatee];
                self.instructions().local_get(index);
            }
            Expr::Num(n) => {
                self.instructions().i64_const(*n);
                self.box_i64();
            }
            Expr::Bool(b) => {
                self.instructions().i32_const(*b as i32);
                self.box_bool();
            }
            Expr::Lam(params, body) => {
                let mut captures: Vec<_> = expr.free_vars().into_iter().collect();
                // TODO: Determine if there's a better order for the captured variables.
                captures.sort_by_key(|var| self.var_indices[var]);
                for var in &captures {
                    let index = self.var_indices[var];
                    self.instructions().local_get(index);
                }
                self.instructions().array_new_fixed(type_idx::VALUE_ARRAY, captures.len() as u32);
                let func_index = self.manager.push_closure(captures, params, body);
                self.instructions()
                    .ref_func(func_index)
                    .struct_new(type_idx::CLOSURE[params.len()]);
                self.manager.ref_func_targets.insert(func_index);
            }
            Expr::AppClo(clo, args) => {
                let clo_idx = self.var_indices[&clo.locatee];
                let clo_type_idx = type_idx::CLOSURE[args.len()];
                self.instructions()
                    .local_get(clo_idx)
                    .ref_cast_non_null(wasm::HeapType::Concrete(clo_type_idx))
                    .struct_get(clo_type_idx, 0);
                for arg in args {
                    self.compile_expr(&arg.locatee);
                }
                self.instructions()
                    .local_get(clo_idx)
                    .ref_cast_non_null(wasm::HeapType::Concrete(clo_type_idx))
                    .struct_get(clo_type_idx, 1)
                    .call_ref(type_idx::CLOSURE_FUNC[args.len()]);
            }
            Expr::AppFun(func, _types, args) => {
                let func_index = self.manager.func_index(func.locatee);
                for arg in args {
                    self.compile_expr(&arg.locatee);
                }
                self.instructions().call(func_index);
            }
            Expr::BinOp(lhs, op, rhs) => {
                // TODO: Support comparison for all types not only for `Int`.
                self.compile_expr(&lhs.locatee);
                self.unbox_i64();
                self.compile_expr(&rhs.locatee);
                self.unbox_i64();
                self.compile_op_code(*op);
            }
            Expr::Let(var, _typ, expr, tail) => {
                self.compile_expr(&expr.locatee);
                self.intro_local(var.locatee, |this, var_idx| {
                    this.instructions().local_set(var_idx);
                    this.compile_expr(&tail.locatee);
                });
            }
            Expr::If(cond, then, elze) => {
                self.compile_expr(&cond.locatee);
                self.unbox_bool();
                self.instructions().if_(wasm::BlockType::Result(super::ref_type(type_idx::VALUE)));
                self.compile_expr(&then.locatee);
                self.instructions().else_();
                self.compile_expr(&elze.locatee);
                self.instructions().end();
            }
            Expr::Record(fields) => {
                if fields.is_sorted_by_key(|(name, _)| name.locatee.as_str()) {
                    for (_, expr) in fields {
                        self.compile_expr(&expr.locatee);
                    }
                    self.instructions().struct_new(type_idx::RECORD[fields.len()]);
                } else {
                    todo!("large or unsorted records are not yet supported");
                }
            }
            Expr::Proj(record, _field, index_size) => {
                let (index, size) = index_size.expect("projection without index");
                let size = size as usize;
                let record_type_idx = type_idx::RECORD[size];
                self.compile_expr(&record.locatee);
                self.instructions().ref_cast_non_null(wasm::HeapType::Concrete(record_type_idx));
                self.instructions().struct_get(record_type_idx, index);
            }
            Expr::Variant(_constr, rank, payload) => {
                let rank = rank.expect("ranks are inserted during type checking");
                self.instructions().i32_const(rank as i32);
                if let Some(payload) = payload {
                    self.compile_expr(&payload.locatee);
                } else {
                    self.instructions().ref_null(wasm::HeapType::Concrete(type_idx::VALUE));
                }
                self.instructions().struct_new(type_idx::VARIANT);
            }
            Expr::Match(scrut, branches) => {
                // TODO: We should store the rank in patterns during type checking.
                let degree = branches.len() as u32;
                let mut branches: Vec<&_> = branches.iter().collect();
                branches.sort_by_key(|branch| branch.pattern.locatee.constr.as_str());
                for _ in 0..degree + 2 {
                    self.instructions()
                        .block(wasm::BlockType::Result(super::ref_type(type_idx::VALUE)));
                }
                self.compile_expr(&scrut.locatee);
                self.intro_local(ExprVar::new("$scrutinee"), |this, scrut_idx| {
                    this.instructions().local_tee(scrut_idx).local_get(scrut_idx);
                });
                self.instructions()
                    .ref_cast_non_null(wasm::HeapType::Concrete(type_idx::VARIANT))
                    .struct_get(type_idx::VARIANT, 0)
                    .br_table(0..degree, degree)
                    .end();
                for (rank, branch) in branches.iter().enumerate() {
                    if let Some(binder) = branch.pattern.locatee.binder {
                        self.instructions()
                            .ref_cast_non_null(wasm::HeapType::Concrete(type_idx::VARIANT))
                            .struct_get(type_idx::VARIANT, 1)
                            .ref_as_non_null();
                        self.intro_local(binder.locatee, |this, binder_idx| {
                            this.instructions().local_set(binder_idx);
                            this.compile_expr(&branch.rhs.locatee);
                        });
                    } else {
                        self.instructions().drop();
                        self.compile_expr(&branch.rhs.locatee);
                    }
                    self.instructions().br(degree - rank as u32).end();
                }
                self.instructions().unreachable().end();
            }
        }
    }

    fn compile_op_code(&mut self, op: OpCode) {
        match op {
            OpCode::Add => {
                self.instructions().i64_add();
                self.box_i64();
            }
            OpCode::Sub => {
                self.instructions().i64_sub();
                self.box_i64();
            }
            OpCode::Mul => {
                self.instructions().i64_mul();
                self.box_i64();
            }
            OpCode::Div => {
                self.instructions().i64_div_s();
                self.box_i64();
            }
            OpCode::Equals => {
                self.instructions().i64_eq();
                self.box_bool();
            }
            OpCode::NotEq => {
                self.instructions().i64_ne();
                self.box_bool();
            }
            OpCode::Less => {
                self.instructions().i64_lt_s();
                self.box_bool();
            }
            OpCode::LessEq => {
                self.instructions().i64_le_s();
                self.box_bool();
            }
            OpCode::Greater => {
                self.instructions().i64_gt_s();
                self.box_bool();
            }
            OpCode::GreaterEq => {
                self.instructions().i64_ge_s();
                self.box_bool();
            }
        }
    }

    fn box_i64(&mut self) {
        self.instructions().struct_new(type_idx::INT);
    }

    fn unbox_i64(&mut self) {
        self.instructions()
            .ref_cast_non_null(wasm::HeapType::Concrete(type_idx::INT))
            .struct_get(type_idx::INT, 0);
    }

    fn box_bool(&mut self) {
        self.instructions().struct_new(type_idx::BOOL);
    }

    fn unbox_bool(&mut self) {
        self.instructions()
            .ref_cast_non_null(wasm::HeapType::Concrete(type_idx::BOOL))
            .struct_get(type_idx::BOOL, 0);
    }
}
