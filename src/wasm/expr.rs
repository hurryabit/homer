use std::collections::HashMap;

use wasm_encoder::BlockType;
use wasm_encoder::Function;
use wasm_encoder::HeapType;
use wasm_encoder::InstructionSink;

use super::type_idx;
use crate::syntax::Expr;
use crate::syntax::ExprVar;
use crate::syntax::FuncDecl;
use crate::syntax::OpCode;

pub(crate) struct FuncCompiler<'a> {
    func: &'a FuncDecl,
    func_indices: &'a HashMap<ExprVar, u32>,
    local_vars: Vec<ExprVar>,
    max_num_local_vars: usize,
    var_indices: HashMap<ExprVar, u32>,
    bytes: Vec<u8>,
}

impl<'a> FuncCompiler<'a> {
    pub(crate) fn new(func: &'a FuncDecl, func_indices: &'a HashMap<ExprVar, u32>) -> Self {
        let local_vars: Vec<_> = func.expr_params.iter().map(|(name, _)| name.locatee).collect();
        let max_num_local_vars = local_vars.len();
        let var_indices =
            local_vars.iter().enumerate().map(|(index, var)| (*var, index as u32)).collect();
        let bytes = Vec::new();
        Self { func, func_indices, local_vars, max_num_local_vars, var_indices, bytes }
    }

    pub(crate) fn compile(mut self) -> Function {
        self.compile_expr(&self.func.body.locatee);
        self.instructions().end();
        let extra_locals = self.max_num_local_vars - self.func.expr_params.len();
        let mut func = Function::new([(extra_locals as u32, super::ref_type(type_idx::VALUE))]);
        func.raw(self.bytes);
        func
    }

    fn instructions(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.bytes)
    }

    fn intro_var(&mut self, var: ExprVar, f: impl FnOnce(&mut Self, u32)) {
        let index = self.local_vars.len() as u32;
        let old_index = self.var_indices.insert(var, index);
        self.local_vars.push(var);
        self.max_num_local_vars = self.max_num_local_vars.max(self.local_vars.len());
        f(self, index);
        self.local_vars.pop();
        if let Some(old_index) = old_index {
            self.var_indices.insert(var, old_index);
        } else {
            self.var_indices.remove(&var);
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Error => {
                // TODO: Determine what we should throw here.
                self.instructions().throw(0);
            }
            Expr::Var(var) => {
                let index = self.var_indices[&var.locatee];
                self.instructions().local_get(index);
            }
            Expr::Num(n) => {
                self.instructions().i64_const(*n);
            }
            Expr::Bool(b) => {
                self.instructions().i32_const(*b as i32);
            }
            Expr::Lam(..) => todo!(),
            Expr::AppClo(..) => todo!(),
            Expr::AppFun(func, _types, args) => {
                let func_index = self.func_indices[&func.locatee];
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
                self.intro_var(var.locatee, |this, index| {
                    this.instructions().local_set(index);
                    this.compile_expr(&tail.locatee);
                });
            }
            Expr::If(cond, then, elze) => {
                self.compile_expr(&cond.locatee);
                self.unbox_bool();
                self.instructions().if_(BlockType::Result(super::ref_type(type_idx::VALUE)));
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
                self.instructions().ref_cast_non_null(HeapType::Concrete(record_type_idx));
                self.instructions().struct_get(record_type_idx, index);
            }
            Expr::Variant(..) => todo!(),
            Expr::Match(..) => todo!(),
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
            .ref_cast_non_null(HeapType::Concrete(type_idx::INT))
            .struct_get(type_idx::INT, 0);
    }

    fn box_bool(&mut self) {
        self.instructions().struct_new(type_idx::BOOL);
    }

    fn unbox_bool(&mut self) {
        self.instructions()
            .ref_cast_non_null(HeapType::Concrete(type_idx::BOOL))
            .struct_get(type_idx::BOOL, 0);
    }
}
