use crate::*;
use anf::*;
use parity_wasm::builder;
use parity_wasm::elements;
use parity_wasm::elements::Instruction::*;
use std::rc::Rc;

pub fn gen_module(module: &anf::Module) -> Result<elements::Module, String> {
    let mut builder = builder::module();
    let runtime_module = parity_wasm::deserialize_file("runtime/runtime.wasm").unwrap();
    let mut runtime_funcs = im::HashMap::new();

    // Push all the function type signatures from the runtime module into the module we're building.
    for typ in runtime_module.type_section().unwrap().types() {
        match typ {
            elements::Type::Function(ftyp) => {
                builder.push_signature(
                    builder::signature()
                    .with_params(ftyp.params().into())
                    .with_results(ftyp.results().into())
                    .build_sig()
                );
            },
        }
    }

    let funcs = runtime_module.function_section().unwrap().entries();
    println!("funcs({}): {:?}", funcs.len(), funcs);
    println!("exports({}): {:?}", runtime_module.export_section().unwrap().entries().len(), runtime_module.export_section().unwrap().entries());
    // Push imports for all the exported runtime functions into the module we're building.
    for export in runtime_module.export_section().unwrap().entries() {
        match export.internal() {
            elements::Internal::Function(idx) => {
                // FIXME: ehh, isn't there a better way to look up the func?
                let func = &funcs[*idx as usize - runtime_module.import_count(elements::ImportCountType::Function)];
                let import = 
                    builder::import()
                        .module("runtime")
                        .field(export.field())
                        .external()
                        .func(func.type_ref())
                        .build();

                runtime_funcs.insert(
                    export.field(),
                    builder.push_import(import));
            }
            _ => ()
        }
    }

    let num_runtime_funcs = runtime_funcs.len() as u32;

    let mut fungen = Fungen{
        instrs: Vec::new(), 
        runtime_funcs: runtime_funcs,
        closures: Vec::new(),
        proc_sig:  builder.push_signature(builder::signature().build_sig()),
        table_index: 0,
    };

    // Compile all top-level functions. Encountered anonymous functions are
    // pushed into context.closures.
    for decl in &module.func_decls {
        let instrs = fungen.gen_fun(
            &decl.body, 
            decl.params.len(), 
            decl.body.bindings.len());

        let loc = builder.push_function(
            builder::function()
                .signature()
                .build()
                .body()
                .with_instructions(elements::Instructions::new(instrs))
                .build()
                .build());

        builder.push_export(
            self::builder::export()
            .field(decl.name.as_str())
            .internal()
            .func(num_runtime_funcs + loc.body)
            .build());
    }

    // Now compile the lifted lambdas and create the function table (for indirect calls). We do this after the
    // top-level functions have been compiled in order to have predictable indices for the top-level functions
    // for direct calls.
    let mut table_entries = Vec::new();
    loop {
        let mut closures = std::mem::replace(&mut fungen.closures, Vec::new());    
        if closures.is_empty() {
            break
        }

        for (table_index, closure) in closures {
            let instrs = fungen.gen_fun(
                &closure.body, 
                closure.params.len() + closure.captured.len(), 
                closure.body.bindings.len());

            let loc = builder.push_function(
                builder::function()
                    .signature().build()
                    .body()
                    .with_instructions(elements::Instructions::new(instrs))
                    .build()
                    .build());

            assert!(table_index as usize == table_entries.len());
            table_entries.push(num_runtime_funcs + loc.body);
        }
    }

    builder.push_table(
        builder::TableBuilder::new()
            .with_min(table_entries.len() as u32)
            .with_element(0, table_entries).build()
    );
    
    Ok(builder.build())
}

struct Fungen<'a> {
    instrs: Vec<elements::Instruction>,
    runtime_funcs: im::HashMap<&'a str, u32>,
    proc_sig: u32, // FIXME yuck
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

    pub fn gen_fun(&mut self, expr: &'a Expr, nargs: usize, nvars: usize) -> Vec<elements::Instruction> {
        self.gen_expr(expr);
    
        let num_to_pop = (nargs + nvars) as i32 - 1 /* except last binding, i.e. the return value */;
        if num_to_pop > 0 {
          self.emit(I32Const(num_to_pop));
          self.call_runtime("ret");
        }
        self.emit(End);
    
        std::mem::replace(&mut self.instrs, Vec::new())
    }
    

    fn emit(&mut self, instr: elements::Instruction) {
        self.instrs.push(instr);
    }

    fn call_runtime(&mut self, fun: &str) {
        self.emit(Call(
            *self.runtime_funcs.get(fun)
                .expect(&format!("call_runtime: {} is not known!", fun))));
    }

    pub fn get_atom(&mut self, atom: &Atom) {
        self.emit(I32Const(atom.0.0 as i32));
        self.call_runtime("load");
    }

    pub fn gen_expr(&mut self, expr: &'a Expr) {
        for binding in &expr.bindings {
            match &binding.bindee {
                Bindee::Error(_) => 
                    self.call_runtime("error"),
                Bindee::Atom(a) => 
                    self.get_atom(a),
                Bindee::Num(n) => {
                    self.emit(I64Const(*n));
                    self.call_runtime("alloc_i64");
                }
                Bindee::Bool(b) => {
                    self.emit(I32Const(if *b { 1 } else { 0 }));
                    self.call_runtime("alloc_i64"); // TODO: Tags for true/false?
                }
                Bindee::MakeClosure(mk_clo) => {
                    let table_index = self.push_closure(mk_clo);

                    // Push the table index allocated to the closure to the WASM stack.
                    self.emit(I32Const(table_index as i32));

                    // Push the indices of the captured variables to the WASM stack.
                    for IdxVar(idx, _) in &mk_clo.captured {
                        self.emit(I32Const(*idx as i32 - 1));
                    }
                    // ... and invoke the right runtime function to allocate the closure. 
                    // Runtime takes care of copying the variables into the closure.
                    // TODO: Perhaps makes sense to reconsider this approach or at least have a generic fallback?

                    self.call_runtime(&format!("alloc_closure_{}", mk_clo.captured.len()));
                }
                Bindee::AppClosure(Atom(IdxVar(idx, _)), params) => {
                    // Push the closure stack offset to WASM stack.
                    self.emit(I32Const(*idx as i32 - 1));

                    // Invoke PrepAppClosure to copy variables from the closure to top of stack
                    // and retrieve the function index.
                    self.call_runtime("prep_app_closure");

                    for (Atom(IdxVar(idx, _)), offset) in params.iter().zip(0..) {
                        // TODO: If we wouldn't need to push the closure arguments onto the stack
                        // then we could be smarter here and instead avoid unnecessary shuffling
                        // of the stack, e.g. if the arguments are already on top of stack we don't
                        // need to copy them around (though we'll then need to make sure we don't
                        // pop them).
                        self.emit(I32Const((idx + offset - 1) as i32));

                        // Call PushParam which will push the argument to the top of the stack
                        // from given index, offsetting the captured variables.
                        self.call_runtime("push_param")
                    }

                    // Invoke the function.
                    self.emit(CallIndirect(self.proc_sig, 0 /* table */));
                }

                Bindee::AppFunc(index, _name, args) => {
                    // FIXME tail-call optimization. If this is the last binding we can
                    // shift the arguments over the frame.
                    for arg in args {
                        self.get_atom(arg);
                    }

                    // Emit a direct call. Index is offset by the runtime functions
                    // that have been imported.
                    self.emit(Call(*index + self.runtime_funcs.len() as u32));
                }

                Bindee::BinOp(lhs, op, rhs) => {
                    // Push the operand indices to the WASM stack. The runtime functions will
                    // load them from the homer stack and push the result to homer stack.
                    self.emit(I32Const(lhs.0.0 as i32 - 1));
                    self.emit(I32Const(rhs.0.0 as i32 - 1));
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
                    self.call_runtime("deref_i64");
                    self.emit(If(elements::BlockType::NoResult));
                    self.gen_expr(&then);
                    self.emit(Else);
                    self.gen_expr(&elze);
                    self.emit(End);
                }

                Bindee::Record(_, _) => panic!("TODO record"),
                Bindee::Project(_, _, _) => panic!("TODO project"),
                Bindee::Variant(rank, _constr, payload) => {
                    self.emit(I32Const(*rank as i32));
                    if let Some(atom) = payload {
                        self.emit(I32Const(atom.0.0 as i32 - 1));
                    } else {
                        // TODO a bit messy, perhaps better would be to have to variant tags.
                        self.emit(I32Const(-1));
                    }
                    self.call_runtime("alloc_variant");
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
                    self.emit(I32Const(scrut.0.0 as i32 - 1));
                    self.call_runtime("get_rank");
                    let table_data = Box::new(elements::BrTableData{
                        table: (0 .. (branches.len() as u32)).collect::<Vec<u32>>().into_boxed_slice(),
                        default: branches.len() as u32, // FIXME should make it crash rather than nop
                    });
                    self.emit(BrTable(table_data));
                    self.emit(End);

                    // Emit the branch bodies
                    for (branch, index) in branches.iter().zip(0..) {
                        self.gen_expr(&branch.rhs);
                        // Branch to outer block, if needed.
                        let block = branches.len() as u32 - 1 - index;
                        if block > 0 {
                            self.emit(Br(branches.len() as u32 - 1 - index));
                        }
                        self.emit(End);
                    }

                }
            }
        }
    }
}