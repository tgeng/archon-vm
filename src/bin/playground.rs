use std::fs::File;
use std::io::{BufWriter};
use cranelift::codegen;
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift::prelude::{AbiParam, Block, Configurable, InstBuilder, Signature};
use cranelift::prelude::types::I64;
use cranelift_module::{FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

struct Compiler {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: ObjectModule,
    functions: Vec<FunctionDefinition>,
}

struct FunctionDefinition {
    name: String,
    func_id: FuncId,
    clir: String,
}

impl Default for Compiler {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("preserve_frame_pointers", "true").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = ObjectBuilder::new(isa, "playground", cranelift_module::default_libcall_names()).unwrap();
        let module = ObjectModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            functions: Vec::new(),
        }
    }
}

impl Compiler {
    fn declare_function(&mut self, name: &str, sig: &Signature) -> FuncId {
        self.module.declare_function(name, cranelift_module::Linkage::Local, sig).unwrap()
    }

    fn define_function<F>(&mut self, name: &str, sig: &Signature, f: F) -> FuncId where F: FnOnce(&mut ObjectModule, &mut FunctionBuilder, Block) {
        let func_id = self.module.declare_function(name, cranelift_module::Linkage::Local, sig).unwrap();
        self.ctx.func.signature = sig.clone();
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.seal_block(entry_block);
        builder.switch_to_block(entry_block);
        f(&mut self.module, &mut builder, entry_block);
        builder.finalize();
        let clir = self.ctx.func.display().to_string();
        self.module.define_function(func_id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.functions.push(FunctionDefinition {
            name: name.to_owned(),
            func_id,
            clir,
        });
        func_id
    }

    fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

fn main() {
    let mut compiler = Compiler::default();
    let mut sig = compiler.module.make_signature();
    sig.call_conv = CallConv::Tail;
    sig.params.push(AbiParam::new(I64));
    sig.returns.push(AbiParam::new(I64));
    let bar = compiler.define_function("bar", &sig, |module, builder, entry_block| {
        let first_arg = builder.block_params(entry_block)[0];
        let one = builder.ins().iconst(I64, 1);
        let result = builder.ins().iadd(first_arg, one);
        builder.ins().return_(&[result]);
    });
    compiler.define_function("foo", &sig, |module, builder, entry_block| {
        let first_arg = builder.block_params(entry_block)[0];
        let bar_func_ref = module.declare_func_in_func(bar, builder.func);
        builder.ins().return_call(bar_func_ref, &[first_arg]);
    });
    let object_product = compiler.finish();
    let output_path = home::home_dir().unwrap().join("tmp/playground.o");
    let temp_output_writer = BufWriter::new(File::create(output_path).unwrap());
    object_product.object.write_stream(temp_output_writer).unwrap();
}