
use std::collections;

use crate::*;
use syntax::*;
use syntax::Type::*;
use parity_wasm::builder;
use parity_wasm::elements;


pub fn gen_module(module: &syntax::Module) -> Result<elements::Module, String> {
    let mut builder = builder::module();    

    for decl in &module.decls {
        match decl {
            Decl::Func(fun_decl) => {
                builder.push_function(gen_fun_decl(&fun_decl));
            },
            Decl::Type(_) => ()
        }
    }
    Ok(builder.build())
}


pub fn gen_fun_decl(fun_decl: &FuncDecl) -> builder::FunctionDefinition {
    builder::function()
      .signature().build()
      .body()
      .build()
      .build()
}
        /*
        // Add function parameters to environment
        let mut vars = collections::HashMap::new();
        let mut index = 0;
        for (i, (lvar, ltyp)) in fun_decl.expr_params.iter().enumerate() {
            let typ = convert_type(t_ptr, &ltyp.locatee);
            let val = builder.block_params(entry_block)[i];
            let var = Variable::new(index);
            index += 1;
            builder.declare_var(var, typ);
            builder.def_var(var, val);
            vars.insert(lvar.locatee, var);       
            // TODO: Track the variable index in block
        }
        
        let mut fungen = 
            Fungen { 
                builder: builder,
                vars: vars,
                //module: &mut self.module,
            };
        let body = fungen.gen_expr(&fun_decl.body.locatee);

        fungen.builder.ins().return_(&[body]);
        fungen.builder.finalize();

        let ident = self.module
            .declare_function(fun_decl.name.locatee.as_str(), Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(ident, &mut self.ctx, &mut codegen::binemit::NullTrapSink {})
            .map_err(|e| e.to_string())?;
        self.module.clear_context(&mut self.ctx);

        Ok(())
    }
}*/

/*
struct Fungen<'a> {
    builder: FunctionBuilder<'a>,
    vars: collections::HashMap<ExprVar, Variable>,
    //module: &'a mut SimpleJITModule,
}

impl<'a> Fungen<'a> {
    pub fn gen_expr(&mut self, e: &Expr) -> Value {
        match e {
            Expr::Num(n) =>
                self.builder.ins().iconst(types::I64, *n),

            Expr::Bool(b) =>
                self.builder
                  .ins()
                  .iconst(types::I64,
                    if *b { 1 } else { 0 }),

            Expr::Var(expr_var) => {
                let v = self.vars.get(&expr_var).expect("unknown variable");
                self.builder.use_var(*v)
            },

            Expr::BinOp(lhs_expr, op, rhs_expr) => {
                let lhs = self.gen_expr(&lhs_expr.locatee);
                let rhs = self.gen_expr(&rhs_expr.locatee);
                match op {
                    OpCode::Add => self.builder.ins().iadd(lhs, rhs),
                    OpCode::Sub => self.builder.ins().isub(lhs, rhs),
                    OpCode::Mul => self.builder.ins().imul(lhs, rhs),
                    OpCode::Div => self.builder.ins().udiv(lhs, rhs),
                    OpCode::Equals => self.gen_cmp(IntCC::Equal, lhs, rhs),
                    OpCode::NotEq => self.gen_cmp(IntCC::NotEqual, lhs, rhs),
                    OpCode::Less => self.gen_cmp(IntCC::SignedLessThan, lhs, rhs),
                    OpCode::LessEq => self.gen_cmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
                    OpCode::Greater => self.gen_cmp(IntCC::SignedGreaterThan, lhs, rhs),
                    OpCode::GreaterEq => self.gen_cmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
                }
            },

            _ =>
                panic!("TODO")

            
        }
    }

    pub fn gen_cmp(&mut self, cmp: IntCC, lhs: Value, rhs: Value) -> Value {
        let cmp = self.builder.ins().icmp(cmp, lhs, rhs);
        self.builder.ins().bint(types::I64, cmp)
    }

}

fn convert_type(ptr_t: types::Type, typ: &syntax::Type) -> types::Type {
    match typ {
        Int => types::I64,
        Bool => types::B1,
        Fun(_, _) => ptr_t,
        Record(_) => ptr_t,
        Variant(_) => ptr_t,
        Var(t_var) => match t_var.as_str() {
            "Int" => types::I64,
            "Bool" => types::B1,
            _ => panic!("type variable: {}", t_var)
        },
        SynApp(_, _) => panic!("type application: {:?}", typ),
        Error => panic!("unexpected Type.Error")
    }
}

#[test]
fn test_example() {
    let input = "fn main(x: Int) -> Int { 10 * x }";
    let humanizer = &location::Humanizer::new(input);
    let (opt_module, _) = 
        syntax::Module::parse(input, &humanizer);
    let module = opt_module.unwrap();
    module.check(&humanizer).unwrap();
    println!("module:\n{:?}", module);

    let mut codegen = Codegen::default();
    let ptr_to_main = codegen.gen_module(&module).unwrap();

    unsafe {
        let main = core::mem::transmute::<_, fn(i64) -> i64>(ptr_to_main);
        let result = main(3);
        println!("result: {:?}", result);
    }
}*/