use crate::*;
use anf::*;
use parity_wasm::builder;
use parity_wasm::elements;
use parity_wasm::elements::Deserialize;
use parity_wasm::elements::Instruction::*;

pub fn gen_module(module: &anf::Module) -> Result<elements::Module, String> {
    let mut runtime_bytes = &include_bytes!("../../../runtime/runtime.wasm")[..];
    let mut runtime_module =
        parity_wasm::elements::Module::deserialize(&mut runtime_bytes).unwrap();

    // Build the runtime functions map.
    let mut runtime_funcs: im::HashMap<String, u32> = im::HashMap::new();
    let num_imports = runtime_module.import_count(elements::ImportCountType::Function) as u32;
    for export in runtime_module.export_section().unwrap().entries() {
        match export.internal() {
            elements::Internal::Function(idx) => {
                runtime_funcs.insert(String::from(export.field()), *idx as u32);
            }
            _ => (),
        }
    }

    // The offset to calling any top-level function, e.g. it's the
    // number of imported and defined functions already in the module
    let call_offset = num_imports + runtime_funcs.len() as u32;

    // Clear the existing table. TODO: Add to the existing table instead.
    runtime_module.table_section_mut().unwrap().entries_mut().clear();

    // TODO: Clean out unnecessary items from runtime exports.

    // Create a new builder that extends from the runtime module.
    let mut builder = builder::from_module(runtime_module);

    let mut fungen = Fungen {
        instrs: Vec::new(),
        runtime_funcs: runtime_funcs,
        closures: Vec::new(),
        proc_sig: builder.push_signature(builder::signature().build_sig()),
        call_offset: call_offset,
        table_index: 0,
    };

    // Compile all top-level functions. Encountered anonymous functions are
    // pushed into context.closures.
    for decl in &module.func_decls {
        let instrs = fungen.gen_fun(&decl.body, decl.params.len());

        let loc = builder.push_function(
            builder::function()
                .signature()
                .build()
                .body()
                .with_instructions(elements::Instructions::new(instrs))
                .build()
                .build(),
        );

        builder.push_export(
            self::builder::export()
                .field(format!("${}", decl.name).as_str())
                // FIXME: ^ would be nicer to mangle the runtime functions instead!
                .internal()
                .func(loc.body + num_imports)
                .build(),
        );
    }

    // Now compile the lifted lambdas and create the function table (for indirect calls). We do this after the
    // top-level functions have been compiled in order to have predictable indices for the top-level functions
    // for direct calls.
    let mut table_entries = Vec::new();
    loop {
        let closures = std::mem::replace(&mut fungen.closures, Vec::new());
        if closures.is_empty() {
            break;
        }

        for (table_index, closure) in closures {
            let instrs =
                fungen.gen_fun(&closure.body, closure.params.len() + closure.captured.len());

            let loc = builder.push_function(
                builder::function()
                    .signature()
                    .build()
                    .body()
                    .with_instructions(elements::Instructions::new(instrs))
                    .build()
                    .build(),
            );

            assert!(table_index as usize == table_entries.len());
            table_entries.push(loc.body + num_imports);
        }
    }

    builder.push_table(
        builder::TableBuilder::new()
            .with_min(table_entries.len() as u32)
            .with_element(0, table_entries)
            .build(),
    );

    Ok(builder.build())
}

struct Fungen<'a> {
    instrs: Vec<elements::Instruction>,
    runtime_funcs: im::HashMap<String, u32>,
    proc_sig: u32, // FIXME yuck
    call_offset: u32,
    table_index: u32,
    closures: Vec<(u32, &'a MakeClosure)>,
}

impl<'a> Fungen<'a> {
    fn push_closure(&mut self, mk_clo: &'a MakeClosure) -> u32 {
        let table_index = self.table_index;
        self.table_index += 1;
        self.closures.push((table_index, mk_clo));
        table_index
    }

    pub fn gen_fun(&mut self, expr: &'a Expr, nargs: usize) -> Vec<elements::Instruction> {
        self.gen_expr(expr, Some(nargs as i32));
        self.emit(End);

        std::mem::replace(&mut self.instrs, Vec::new())
    }

    fn emit(&mut self, instr: elements::Instruction) {
        self.instrs.push(instr);
    }

    fn call_runtime(&mut self, fun: &str) {
        self.emit(Call(
            *self.runtime_funcs.get(fun).expect(&format!("call_runtime: {} is not known!", fun)),
        ));
    }

    pub fn get_atom(&mut self, atom: &Atom) {
        self.emit(I32Const(atom.get_index() as i32 - 1));
        self.call_runtime("load");
    }

    pub fn gen_expr(&mut self, expr: &'a Expr, tail: Option<i32>) {
        assert!(!expr.bindings.is_empty());

        let num_bindings = expr.bindings.len();
        for binding in &expr.bindings[0..num_bindings - 1] {
            self.gen_binding(&binding, None);
        }

        let num_to_pop = tail.unwrap_or(0) + num_bindings as i32 - 1;
        let last_binding = expr.bindings.last().unwrap();
        self.gen_binding(&last_binding, Some(num_to_pop));
    }

    fn gen_binding(&mut self, binding: &'a Binding, tail: Option<i32>) {
        match &binding.bindee {
            Bindee::Error(_) => self.call_runtime("error"),
            Bindee::Atom(a) => self.get_atom(a),
            Bindee::Num(n) => {
                self.emit(I64Const(*n));
                self.call_runtime("alloc_i64");
            }
            Bindee::Bool(b) => {
                self.emit(I64Const(if *b { 1 } else { 0 }));
                self.call_runtime("alloc_i64");
            }
            Bindee::MakeClosure(mk_clo) => {
                // Defer the compilation of the lambda body, allocating its function
                // table index for the indirect call.
                let table_index = self.push_closure(mk_clo);
                // Allocate the closure.
                self.emit(I32Const(table_index as i32));
                self.emit(I32Const(mk_clo.captured.len() as i32));
                self.call_runtime("alloc_closure");
                // Capture the variables by setting each variable in the closure
                // from the stack.
                for (IdxVar(idx, _), var) in mk_clo.captured.iter().zip(0..) {
                    self.emit(I32Const(var));
                    self.emit(I32Const(*idx as i32));
                    self.call_runtime("set_var");
                }
            }
            Bindee::AppClosure(clo, params) => {
                self.gen_app_closure(clo, params, tail);
                if tail.is_some() {
                    return;
                }
            }

            Bindee::AppFunc(index, _name, args) => {
                self.gen_app_func(*index, args, tail);
                if tail.is_some() {
                    return;
                }
            }

            Bindee::BinOp(lhs, op, rhs) => {
                // Push the operand indices to the WASM stack. The runtime functions will
                // load them from the homer stack and push the result to homer stack.
                self.emit(I32Const(lhs.get_index() as i32 - 1));
                self.emit(I32Const(rhs.get_index() as i32 - 1));
                match op {
                    OpCode::Add => self.call_runtime("add"),
                    OpCode::Sub => self.call_runtime("sub"),
                    OpCode::Mul => self.call_runtime("mul"),
                    OpCode::Div => self.call_runtime("div"),
                    OpCode::Equals => self.call_runtime("equals"),
                    OpCode::NotEq => self.call_runtime("not_eq"),
                    OpCode::Less => self.call_runtime("less"),
                    OpCode::LessEq => self.call_runtime("less_eq"),
                    OpCode::Greater => self.call_runtime("greater"),
                    OpCode::GreaterEq => self.call_runtime("greater_eq"),
                }
            }
            Bindee::If(cond, then, elze) => {
                self.get_atom(cond);
                self.call_runtime("deref_i32");
                self.emit(If(elements::BlockType::NoResult));
                self.gen_expr(&then, tail);
                self.emit(Else);
                self.gen_expr(&elze, tail);
                self.emit(End);

                // FIXME: Figure out a nicer way.
                if tail.is_some() {
                    return;
                }
            }
            Bindee::Record(_, values) => {
                self.emit(I32Const(values.len() as i32));
                self.call_runtime("alloc_record");

                for (Atom(IdxVar(idx, _)), field) in values.iter().zip(0..) {
                    self.emit(I32Const(field));
                    self.emit(I32Const(*idx as i32));
                    self.call_runtime("set_field");
                }
            }
            Bindee::Project(record, index, _) => {
                self.emit(I32Const(record.get_index() as i32 - 1));
                self.emit(I32Const(*index as i32));
                self.call_runtime("get_field");
            }

            Bindee::Variant(rank, _constr, payload) => {
                self.emit(I32Const(*rank as i32));
                if let Some(atom) = payload {
                    self.emit(I32Const(atom.get_index() as i32 - 1));
                    self.call_runtime("alloc_variant");
                } else {
                    self.call_runtime("alloc_variant_0");
                }
            }

            Bindee::Match(scrut, branches) => {
                // block $n
                //   block $n-1
                //     ...
                //       block $0
                //         <call runtime.get_variant_rank>
                //         br_table $0 $1 $2 ... $n
                //         <code for first branch>
                //         return
                //     <code for second branch>
                //     return
                //   ...
                // <code for last branch>
                // <return>
                // Emit the start of a block for each branch
                for _ in branches {
                    self.emit(Block(elements::BlockType::NoResult))
                }
                // Emit the innermost block in which we dispatch
                self.emit(Block(elements::BlockType::NoResult));
                self.emit(I32Const(scrut.get_index() as i32 - 1));
                self.call_runtime("get_rank");
                let table_data = Box::new(elements::BrTableData {
                    table: (0..(branches.len() as u32)).collect::<Vec<u32>>().into_boxed_slice(),
                    default: branches.len() as u32, // FIXME should make it crash rather than nop
                });
                self.emit(BrTable(table_data));
                self.emit(End);
                // Emit the branch bodies
                for (branch, index) in branches.iter().zip(0..) {
                    let has_binder = branch.pattern.binder.is_some();
                    if has_binder {
                        self.emit(I32Const(scrut.get_index() as i32 - 1));
                        self.call_runtime("load_payload");
                    }
                    self.gen_expr(&branch.rhs, tail);
                    if has_binder {
                        self.emit(I32Const(1));
                        self.call_runtime("ret");
                    }
                    // Branch to outer block, if needed.
                    let block = branches.len() as u32 - 1 - index;
                    if block > 0 {
                        self.emit(Br(branches.len() as u32 - 1 - index));
                    }
                    self.emit(End);
                }

                // FIXME: Figure out a nicer way
                if tail.is_some() {
                    return;
                }
            }
        }
        for num_to_pop in tail {
            if num_to_pop > 0 {
                self.emit(I32Const(num_to_pop));
                self.call_runtime("ret");
            }
        }
    }

    fn gen_app_closure(&mut self, clo: &Atom, params: &Vec<Atom>, tail: Option<i32>) {
        // Push the closure stack offset to WASM stack.
        self.emit(I32Const(clo.get_index() as i32 - 1));
        // Invoke PrepAppClosure to copy variables from the closure to top of stack
        // and retrieve the function index.
        self.call_runtime("prep_app_closure");
        for (Atom(IdxVar(idx, _)), offset) in params.iter().zip(0..) {
            // TODO: If we wouldn't need to push the closure arguments onto the stack
            // then we could be smarter here and instead avoid unnecessary shuffling
            // of the stack, e.g. if the arguments are already on top of stack we don't
            // need to copy them around (though we'll then need to make sure we don't
            // pop them). Or better yet would be if we'd access the variables directly
            // from the closure itself rather than having them pushed onto the stack.
            self.emit(I32Const((idx + offset - 1) as i32));
            // Call PushParam which will push the argument to the top of the stack
            // from given index, offsetting the captured variables.
            self.call_runtime("push_param")
        }

        tail.map(|num_to_pop| self.call_shift(num_to_pop, params.len() as i32));

        // Invoke the function.
        self.emit(CallIndirect(self.proc_sig, 0 /* table */));
    }

    fn gen_app_func(&mut self, index: u32, params: &Vec<Atom>, tail: Option<i32>) {
        for (Atom(IdxVar(idx, _)), off) in params.iter().zip(0..) {
            self.emit(I32Const(*idx as i32 - 1 + off));
            self.call_runtime("load");
        }
        tail.map(|num_to_pop| self.call_shift(num_to_pop, params.len() as i32));
        // TODO: use ReturnCall. Need to fork parity-wasm to add it.
        self.emit(Call(index + self.call_offset));
    }

    fn call_shift(&mut self, num_to_pop: i32, params: i32) {
        if num_to_pop > 0 {
            assert!(params > 0);
            self.emit(I32Const(num_to_pop));
            self.emit(I32Const(params));
            self.call_runtime("shift");
        }
    }
}
