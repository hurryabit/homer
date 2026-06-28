#![allow(dead_code)]
mod expr;

use wasm_encoder::ArrayType;
use wasm_encoder::CodeSection;
use wasm_encoder::CompositeInnerType;
use wasm_encoder::CompositeType;
use wasm_encoder::ElementSection;
use wasm_encoder::Elements;
use wasm_encoder::ExportKind;
use wasm_encoder::ExportSection;
use wasm_encoder::FieldType;
use wasm_encoder::FuncType;
use wasm_encoder::Function;
use wasm_encoder::FunctionSection;
use wasm_encoder::HeapType;
use wasm_encoder::Module;
use wasm_encoder::RefType;
use wasm_encoder::StorageType;
use wasm_encoder::StructType;
use wasm_encoder::SubType;
use wasm_encoder::TypeSection;
use wasm_encoder::ValType;

use crate::syntax;
use crate::wasm::expr::FuncManager;

mod type_idx {
    pub const MAX_ARITY: usize = 5;

    pub const VALUE: u32 = 0;
    pub const INT: u32 = 1;
    pub const BOOL: u32 = 2;
    pub const VALUE_ARRAY: u32 = 3;
    pub const CLOSURE_FUNC: [u32; MAX_ARITY + 1] = [4, 5, 6, 7, 8, 9];
    pub const CLOSURE: [u32; MAX_ARITY + 1] = [10, 11, 12, 13, 14, 15];
    pub const RECORD: [u32; MAX_ARITY + 1] = [16, 17, 18, 19, 20, 21];
    pub const VARIANT: u32 = 22;
    pub const FUNC: [u32; MAX_ARITY + 1] = [23, 24, 25, 26, 27, 28];

    pub const EXPORTS_START: u32 = 29;

    const _: () = assert!(CLOSURE_FUNC[0] == VALUE_ARRAY + 1);
    const _: () = assert!(CLOSURE[0] == CLOSURE_FUNC[MAX_ARITY] + 1);
    const _: () = assert!(RECORD[0] == CLOSURE[MAX_ARITY] + 1);
    const _: () = assert!(VARIANT == RECORD[MAX_ARITY] + 1);
    const _: () = assert!(FUNC[0] == VARIANT + 1);
    const _: () = assert!(EXPORTS_START == FUNC[MAX_ARITY] + 1);
}

const fn ref_type(idx: u32) -> ValType {
    ValType::Ref(RefType { nullable: false, heap_type: HeapType::Concrete(idx) })
}

const fn null_ref_type(idx: u32) -> ValType {
    ValType::Ref(RefType { nullable: true, heap_type: HeapType::Concrete(idx) })
}

const fn composite_type(inner: CompositeInnerType) -> CompositeType {
    CompositeType { inner, shared: false, descriptor: None, describes: None }
}

fn value_type<'a>(fields: impl IntoIterator<Item = &'a StorageType>) -> SubType {
    let fields: Vec<_> =
        fields.into_iter().map(|typ| FieldType { element_type: *typ, mutable: false }).collect();
    let inner = CompositeInnerType::Struct(StructType { fields: fields.into_boxed_slice() });
    SubType {
        is_final: true,
        supertype_idx: Some(type_idx::VALUE),
        composite_type: composite_type(inner),
    }
}

fn array_type(element_type: StorageType) -> SubType {
    let inner = CompositeInnerType::Array(ArrayType(FieldType { element_type, mutable: false }));
    SubType { is_final: true, supertype_idx: None, composite_type: composite_type(inner) }
}

fn closure_func_type(arity: usize) -> SubType {
    let func = FuncType::new(
        std::iter::once(ref_type(type_idx::VALUE_ARRAY))
            .chain(std::iter::repeat_n(ref_type(type_idx::VALUE), arity)),
        [ref_type(type_idx::VALUE)],
    );
    let inner = CompositeInnerType::Func(func);
    SubType { is_final: true, supertype_idx: None, composite_type: composite_type(inner) }
}

fn closure_type(arity: usize) -> SubType {
    value_type(&[
        StorageType::Val(ref_type(type_idx::VALUE_ARRAY)),
        StorageType::Val(ref_type(type_idx::CLOSURE_FUNC[arity])),
    ])
}

fn record_type(arity: usize) -> SubType {
    value_type(std::iter::repeat_n(&StorageType::Val(ref_type(type_idx::VALUE)), arity))
}

fn func_type(arity: usize) -> SubType {
    let func = FuncType::new(
        std::iter::repeat_n(ref_type(type_idx::VALUE), arity),
        [ref_type(type_idx::VALUE)],
    );
    let inner = CompositeInnerType::Func(func);
    SubType { is_final: true, supertype_idx: None, composite_type: composite_type(inner) }
}

fn build_type_section() -> TypeSection {
    let value_group = &[
        SubType { is_final: false, supertype_idx: None, ..value_type(&[]) },
        value_type(&[StorageType::Val(ValType::I64)]), // Int
        value_type(&[StorageType::Val(ValType::I32)]), // Bool
        array_type(StorageType::Val(ref_type(type_idx::VALUE))),
        closure_func_type(0),
        closure_func_type(1),
        closure_func_type(2),
        closure_func_type(3),
        closure_func_type(4),
        closure_func_type(5),
        closure_type(0),
        closure_type(1),
        closure_type(2),
        closure_type(3),
        closure_type(4),
        closure_type(5),
        record_type(0),
        record_type(1),
        record_type(2),
        record_type(3),
        record_type(4),
        record_type(5),
        value_type(&[
            StorageType::Val(ValType::I32),
            StorageType::Val(null_ref_type(type_idx::VALUE)),
        ]), // Variant
        func_type(0),
        func_type(1),
        func_type(2),
        func_type(3),
        func_type(4),
        func_type(5),
    ];

    let mut section = TypeSection::new();
    for subtype in value_group {
        section.ty().subtype(subtype);
    }
    section
}

pub fn compile_module(module: &syntax::Module) -> Vec<u8> {
    let mut types = build_type_section();
    let mut funcs = FunctionSection::new();
    let mut codes = CodeSection::new();
    let mut func_manager = FuncManager::new(module);
    while let Some((func, type_idx)) = func_manager.compile_next() {
        funcs.function(type_idx);
        codes.function(&func);
    }

    let mut elements = ElementSection::new();
    let mut ref_func_targets: Vec<_> = func_manager.finish().into_iter().collect();
    ref_func_targets.sort();
    elements.declared(Elements::Functions(ref_func_targets.into()));

    fn wasm_type_info(typ: &syntax::Type) -> Option<(ValType, u32)> {
        match typ {
            syntax::Type::Int => Some((ValType::I64, type_idx::INT)),
            syntax::Type::Bool => Some((ValType::I32, type_idx::BOOL)),
            _ => None,
        }
    }

    // Export all functions whose param and return types are all `Int` or `Bool`.
    let mut exports = ExportSection::new();
    let mut next_type_idx = type_idx::EXPORTS_START;
    for (func_idx, func) in module.func_decls().enumerate() {
        let Some(result) = wasm_type_info(&func.return_type.locatee) else {
            continue;
        };
        let Some(params) = func
            .expr_params
            .iter()
            .map(|(_, typ)| wasm_type_info(&typ.locatee))
            .collect::<Option<Vec<_>>>()
        else {
            continue;
        };
        let wrapper_idx = funcs.len();
        let mut wrapper = Function::new([]);
        for (param_idx, (_param_type, param_type_idx)) in params.iter().enumerate() {
            wrapper.instructions().local_get(param_idx as u32).struct_new(*param_type_idx);
        }
        wrapper
            .instructions()
            .call(func_idx as u32)
            .ref_cast_non_null(HeapType::Concrete(result.1))
            .struct_get(result.1, 0)
            .end();

        types.ty().function(params.iter().map(|info| info.0), [result.0]);
        funcs.function(next_type_idx);
        next_type_idx += 1;
        codes.function(&wrapper);
        exports.export(func.name.locatee.as_str(), ExportKind::Func, wrapper_idx);
    }

    let mut module = Module::new();
    module.section(&types).section(&funcs).section(&exports).section(&elements).section(&codes);
    module.finish()
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use wasmtime::Config;
    use wasmtime::Engine;
    use wasmtime::Instance;
    use wasmtime::Module;
    use wasmtime::Store;

    use crate::build;
    use crate::build::Compiler as _;

    fn make_engine() -> Engine {
        let mut config = Config::new();
        config.wasm_function_references(true);
        config.wasm_gc(true);
        Engine::new(&config).expect("failed to create engine")
    }

    fn make_module() -> Arc<Vec<u8>> {
        const INPUT: &str = r#"
        fn f(x: Bool, y: Int, z: Int) -> Int {
            if x {
                y * z
            } else {
                y + z
            }
        }

        fn g() -> Int {
            let x = 1;
            let f = fn(y: Int) { x + y };
            f(2)
        }
        "#;

        let db = &mut build::CompilerDB::new();
        let uri = build::Uri::new("test.doh");
        db.set_input(uri, Arc::new(INPUT.to_string()));
        db.with_diagnostics(uri, |diagnostics| {
            let diagnostics = diagnostics.cloned().collect::<Vec<_>>();
            assert!(diagnostics.is_empty(), "parser/checker failed: {diagnostics:?}");
        });
        db.wasm_module(uri).expect("module could not be compiled")
    }

    #[test]
    fn module_is_valid() {
        let bytes = make_module();
        wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
            .validate_all(&bytes)
            .expect("emitted module should be valid Wasm");
    }

    #[test]
    fn module_snapshot() {
        let bytes = make_module();
        let wat = wasmprinter::print_bytes(&*bytes).expect("failed to print module as WAT");
        insta::assert_snapshot!(wat);
    }

    #[test]
    fn module_execution() {
        let engine = make_engine();
        let module = Module::from_binary(&engine, &make_module()).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();

        let f = instance.get_typed_func::<(i32, i64, i64), i64>(&mut store, "f").unwrap();
        let g = instance.get_typed_func::<(), i64>(&mut store, "g").unwrap();

        assert_eq!(f.call(&mut store, (1, 4, 2)).unwrap(), 8);
        assert_eq!(f.call(&mut store, (0, 4, 2)).unwrap(), 6);
        assert_eq!(g.call(&mut store, ()).unwrap(), 3);
    }
}
