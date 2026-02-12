use crate::frontend::ast::{ExternGlobal, Function, Item};
use crate::mir::MirModule;
use crate::sema::Program;
use crate::sema::types::{BuiltinType, Type, TypeDefKind};

mod emitter;

use self::emitter::FnEmitter;

pub struct LLVMModule {
    pub text: String,
}

pub fn emit_llvm(program: &Program) -> Result<LLVMModule, String> {
    let mut codegen = Codegen::new(program);
    codegen.emit_module()?;
    Ok(LLVMModule {
        text: codegen.output,
    })
}

pub fn emit_llvm_from_mir(program: &Program, mir: &MirModule) -> Result<LLVMModule, String> {
    let mut codegen = Codegen::new(program);
    codegen.emit_module_from_mir(mir)?;
    Ok(LLVMModule {
        text: codegen.output,
    })
}

struct Codegen<'a> {
    program: &'a Program,
    output: String,
    globals: Vec<String>,
}

impl<'a> Codegen<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            output: String::new(),
            globals: Vec::new(),
        }
    }

    fn emit_module(&mut self) -> Result<(), String> {
        self.emit_prelude()?;
        self.emit_user_extern_decls()?;
        for item in &self.program.file.items {
            if let Item::Function(func) = item {
                if func.is_extern {
                    continue;
                }
                self.emit_function(func)?;
            }
        }
        self.emit_globals();
        Ok(())
    }

    fn emit_module_from_mir(&mut self, mir: &MirModule) -> Result<(), String> {
        self.emit_prelude()?;
        self.emit_user_extern_decls()?;
        let mut has_main = false;
        for func in &mir.functions {
            if func.name == "main" {
                has_main = true;
                self.emit_mir_function_named(func, "__gost_user_main")?;
            } else {
                self.emit_mir_function(func)?;
            }
        }
        if has_main {
            self.output.push_str("define i32 @main() {\n");
            self.output
                .push_str("  %ret = call i32 @__gost_rt_start(i32 ()* @__gost_user_main)\n");
            self.output.push_str("  ret i32 %ret\n");
            self.output.push_str("}\n\n");
        }
        self.emit_globals();
        Ok(())
    }

    fn emit_prelude(&mut self) -> Result<(), String> {
        self.output.push_str("%string = type { i8*, i64 }\n");
        self.output.push_str("%slice_obj = type opaque\n");
        self.output.push_str("%slice = type { %slice_obj* }\n");
        self.output.push_str("%shared_obj = type opaque\n");
        self.output.push_str("%shared = type { %shared_obj* }\n");
        self.output.push_str("%map_obj = type opaque\n");
        self.output.push_str("%map = type { %map_obj* }\n");
        self.output.push_str("%chan = type opaque\n");
        self.emit_named_types()?;
        self.output.push_str("declare void @__gost_rt_init()\n");
        self.output
            .push_str("declare i32 @__gost_rt_start(i32 ()*)\n");
        self.output
            .push_str("declare void @__gost_println_str(i8*, i64)\n");
        self.output
            .push_str("declare i64 @__gost_string_len(i8*, i64)\n");
        self.output
            .push_str("declare i32 @__gost_string_get(i8*, i64, i64)\n");
        self.output
            .push_str("declare void @__gost_string_slice(%string*, i8*, i64, i64, i64)\n");
        self.output
            .push_str("declare void @__gost_string_concat(%string*, i8*, i64, i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_string_from_byte(%string*, i32)\n");
        self.output
            .push_str("declare %slice_obj* @__gost_slice_new(i64, i64, i64, void (i8*)*)\n");
        self.output
            .push_str("declare void @__gost_slice_drop(%slice_obj*)\n");
        self.output
            .push_str("declare i64 @__gost_slice_len(%slice_obj*)\n");
        self.output
            .push_str("declare i8* @__gost_slice_data(%slice_obj*)\n");
        self.output
            .push_str("declare void @__gost_slice_bounds_check(%slice_obj*, i64)\n");
        self.output
            .push_str("declare void @__gost_slice_push(%slice_obj*, i8*)\n");
        self.output
            .push_str("declare i32 @__gost_slice_pop(%slice_obj*, i8*)\n");
        self.output
            .push_str("declare %shared_obj* @__gost_shared_new(i64, void (i8*)*, i8*)\n");
        self.output
            .push_str("declare void @__gost_shared_inc(%shared_obj*)\n");
        self.output
            .push_str("declare void @__gost_shared_dec(%shared_obj*)\n");
        self.output
            .push_str("declare i8* @__gost_shared_get_ptr(%shared_obj*)\n");
        self.output
            .push_str("declare i32 @__gost_shared_is_unique(%shared_obj*)\n");
        self.output
            .push_str("declare %map_obj* @__gost_map_new(i32, i64, i64)\n");
        self.output
            .push_str("declare i32 @__gost_map_get(%map_obj*, i8*, i8*)\n");
        self.output
            .push_str("declare void @__gost_map_set(%map_obj*, i8*, i8*)\n");
        self.output
            .push_str("declare i32 @__gost_map_del(%map_obj*, i8*)\n");
        self.output
            .push_str("declare i64 @__gost_map_len(%map_obj*)\n");
        self.output
            .push_str("declare void @__gost_map_drop(%map_obj*)\n");
        self.output
            .push_str("declare %chan* @__gost_chan_new(i64, i32)\n");
        self.output
            .push_str("declare void @__gost_chan_retain(%chan*)\n");
        self.output
            .push_str("declare i32 @__gost_chan_send(%chan*, i8*)\n");
        self.output
            .push_str("declare i32 @__gost_chan_can_send(%chan*)\n");
        self.output
            .push_str("declare i32 @__gost_chan_can_recv(%chan*)\n");
        self.output
            .push_str("declare i32 @__gost_chan_recv(%chan*, i8*)\n");
        self.output
            .push_str("declare i32 @__gost_chan_close(%chan*)\n");
        self.output
            .push_str("declare void @__gost_chan_drop(%chan*)\n");
        self.output
            .push_str("declare i32 @__gost_select_wait(%chan**, i32*, i32)\n");
        self.output
            .push_str("declare void @__gost_panic(i8*, i64)\n");
        self.output
            .push_str("declare i8* @__gost_alloc(i64, i64)\n");
        self.output
            .push_str("declare void @__gost_free(i8*, i64, i64)\n");
        self.output
            .push_str("declare i32 @__gost_error_new(i8*, i64)\n");
        self.output
            .push_str("declare i32 @__gost_singleton_acquire(i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_spawn_thread(void (i8*)*, i8*)\n");
        self.output
            .push_str("declare void @__gost_go_spawn(void (i8*)*, i8*)\n");
        self.output
            .push_str("declare %chan* @__gost_after_ms(i64)\n");
        self.output
            .push_str("declare i32 @__gost_net_last_status()\n");
        self.output
            .push_str("declare i32 @__gost_net_last_http_status()\n");
        self.output
            .push_str("declare void @__gost_net_last_error(%string*)\n");
        self.output
            .push_str("declare void @__gost_net_last_peer(%string*)\n");
        self.output
            .push_str("declare i64 @__gost_net_tcp_listen(i8*, i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_tcp_accept(i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_tcp_connect(i8*, i64)\n");
        self.output
            .push_str("declare i32 @__gost_net_tcp_close(i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_tcp_write(i64, i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_net_tcp_read(%string*, i64, i32)\n");
        self.output
            .push_str("declare i64 @__gost_net_udp_bind(i8*, i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_udp_connect(i8*, i64)\n");
        self.output
            .push_str("declare i32 @__gost_net_udp_close(i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_udp_send(i64, i8*, i64)\n");
        self.output
            .push_str("declare i64 @__gost_net_udp_send_to(i64, i8*, i64, i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_net_udp_recv(%string*, i64, i32)\n");
        self.output
            .push_str("declare void @__gost_net_udp_recv_from(%string*, i64, i32)\n");
        self.output
            .push_str("declare i64 @__gost_net_ws_connect(i8*, i64)\n");
        self.output
            .push_str("declare i32 @__gost_net_ws_close(i64)\n");
        self.output
            .push_str("declare i32 @__gost_net_ws_send_text(i64, i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_net_ws_recv_text(%string*, i64)\n");
        self.output
            .push_str("declare void @__gost_net_http_request(%string*, i8*, i64, i8*, i64, i8*, i64, i8*, i64)\n");
        self.output
            .push_str("declare void @__gost_net_http_request_headers(%string*, i8*, i64, i8*, i64, i8*, i64, i8*, i64, i8*, i64)\n");
        self.output.push('\n');
        Ok(())
    }

    fn emit_globals(&mut self) {
        for global in self.globals.drain(..) {
            self.output.push_str(&global);
            self.output.push('\n');
        }
    }

    fn emit_user_extern_decls(&mut self) -> Result<(), String> {
        let mut any = false;
        for item in &self.program.file.items {
            match item {
                Item::Function(func) => {
                    if !func.is_extern {
                        continue;
                    }
                    let sig = self
                        .program
                        .functions
                        .get(&func.name)
                        .ok_or_else(|| format!("missing signature for {}", func.name))?;
                    let ret_ty = llvm_type(&sig.ret)?;
                    let mut params_ir = Vec::new();
                    for param in &sig.params {
                        params_ir.push(llvm_type(param)?);
                    }
                    if sig.is_variadic {
                        params_ir.push("...".to_string());
                    }
                    let cc = llvm_call_conv(sig.extern_abi.as_deref());
                    self.output.push_str(&format!(
                        "declare {}{} @{}({})\n",
                        cc,
                        ret_ty,
                        func.name,
                        params_ir.join(", ")
                    ));
                    any = true;
                }
                Item::ExternGlobal(global) => {
                    self.emit_user_extern_global_decl(global)?;
                    any = true;
                }
                _ => {}
            }
        }
        if any {
            self.output.push('\n');
        }
        Ok(())
    }

    fn emit_user_extern_global_decl(&mut self, global: &ExternGlobal) -> Result<(), String> {
        let sig = self
            .program
            .extern_globals
            .get(&global.name)
            .ok_or_else(|| format!("missing extern global signature for {}", global.name))?;
        let ty = llvm_type(&sig.ty)?;
        self.output
            .push_str(&format!("@{} = external global {}\n", global.name, ty));
        Ok(())
    }

    fn emit_function(&mut self, func: &Function) -> Result<(), String> {
        let sig = self
            .program
            .functions
            .get(&func.name)
            .ok_or_else(|| format!("missing signature for {}", func.name))?;
        let ret_ty = llvm_type(&sig.ret)?;
        let mut params_ir = Vec::new();
        for (idx, param) in sig.params.iter().enumerate() {
            params_ir.push(format!("{} %arg{}", llvm_type(param)?, idx));
        }
        self.output
            .push_str(&format!("define {} @{}({}) {{\n", ret_ty, func.name, params_ir.join(", ")));
        let mut emitter = FnEmitter::new(
            &func.name,
            &self.program.functions,
            &self.program.extern_globals,
            &self.program.types,
            sig.ret.clone(),
        );
        emitter.set_mir_expr_types(&self.program.expr_types);
        let mut param_names = Vec::new();
        for param in &func.params {
            param_names.push(param.name.clone());
        }
        emitter.emit_prologue(&sig.params, &param_names)?;
        if func.name == "main" {
            emitter.emit_raw("call void @__gost_rt_init()");
        }
        emitter.emit_block(&func.body)?;
        if !emitter.current_block_terminated() {
            if sig.ret == Type::Builtin(BuiltinType::Unit) {
                emitter.emit_return_value(None)?;
            } else {
                return Err(format!("function {} missing return", func.name));
            }
        }
        for block in emitter.blocks {
            self.output.push_str(&format!("{}:\n", block.name));
            for instr in block.instrs {
                self.output.push_str("  ");
                self.output.push_str(&instr);
                self.output.push('\n');
            }
        }
        self.output.push_str("}\n\n");
        for extra in emitter.extra_functions {
            self.output.push_str(&extra);
            self.output.push_str("\n\n");
        }
        for lit in emitter.string_literals {
            self.globals.push(lit);
        }
        Ok(())
    }

    fn emit_mir_function(&mut self, func: &crate::mir::MirFunction) -> Result<(), String> {
        let sig = self
            .program
            .functions
            .get(&func.name)
            .ok_or_else(|| format!("missing signature for {}", func.name))?;
        let ret_ty = llvm_type(&sig.ret)?;
        let mut params_ir = Vec::new();
        for (idx, param) in sig.params.iter().enumerate() {
            params_ir.push(format!("{} %arg{}", llvm_type(param)?, idx));
        }
        self.output
            .push_str(&format!("define {} @{}({}) {{\n", ret_ty, func.name, params_ir.join(", ")));
        let mut emitter = FnEmitter::new(
            &func.name,
            &self.program.functions,
            &self.program.extern_globals,
            &self.program.types,
            sig.ret.clone(),
        );
        let mut param_names = Vec::new();
        if let Some(ast_func) = self.find_function(&func.name) {
            for param in &ast_func.params {
                param_names.push(param.name.clone());
            }
        } else {
            for idx in 0..sig.params.len() {
                param_names.push(format!("arg{}", idx));
            }
        }
        emitter.emit_prologue(&sig.params, &param_names)?;
        emitter.set_mir_mode(true);
        emitter.set_mir_locals(&func.locals)?;
        emitter.set_mir_expr_types(&func.expr_types);
        if func.name == "main" {
            emitter.emit_raw("call void @__gost_rt_init()");
        }
        let mut block_map = Vec::new();
        let mut block_names = Vec::new();
        block_map.push(0);
        block_names.push("entry".to_string());
        for idx in 1..func.blocks.len() {
            let name = format!("mir_bb{}", idx);
            let block_idx = emitter.add_block(name.clone());
            block_map.push(block_idx);
            block_names.push(name);
        }
        for (idx, block) in func.blocks.iter().enumerate() {
            let block_idx = block_map[idx];
            emitter.switch_to(block_idx);
            let term_is_return = matches!(
                block.term,
                crate::mir::Terminator::Return { .. } | crate::mir::Terminator::ReturnError { .. }
            );
            let mut split_idx = block.stmts.len();
            if term_is_return {
                while split_idx > 0 {
                    if matches!(
                        block.stmts[split_idx - 1],
                        crate::mir::MirStmt::Drop { .. }
                            | crate::mir::MirStmt::DropName { .. }
                            | crate::mir::MirStmt::DeferCall { .. }
                            | crate::mir::MirStmt::ExitScope { .. }
                    ) {
                        split_idx -= 1;
                    } else {
                        break;
                    }
                }
            }
            let (prefix, cleanup) = block.stmts.split_at(split_idx);
            for stmt in prefix {
                emitter.emit_mir_stmt(stmt)?;
                if emitter.current_block_terminated() {
                    break;
                }
            }
            if !emitter.current_block_terminated() {
                if term_is_return {
                    match &block.term {
                        crate::mir::Terminator::Return { value } => {
                            let ret_val = if let Some(expr) = value {
                                Some(emitter.emit_expr(expr)?)
                            } else {
                                None
                            };
                            for stmt in cleanup {
                                emitter.emit_mir_stmt(stmt)?;
                            }
                            emitter.emit_return_value(ret_val)?;
                        }
                        crate::mir::Terminator::ReturnError { err } => {
                            let err_val = emitter.emit_expr(err)?;
                            for stmt in cleanup {
                                emitter.emit_mir_stmt(stmt)?;
                            }
                            emitter.emit_error_return_value(err_val)?;
                        }
                        _ => {}
                    }
                } else {
                    emitter.emit_mir_terminator(&block.term, &block_names)?;
                }
            }
        }
        for block in emitter.blocks {
            self.output.push_str(&format!("{}:\n", block.name));
            for instr in block.instrs {
                self.output.push_str("  ");
                self.output.push_str(&instr);
                self.output.push('\n');
            }
        }
        self.output.push_str("}\n\n");
        for extra in emitter.extra_functions {
            self.output.push_str(&extra);
            self.output.push_str("\n\n");
        }
        for lit in emitter.string_literals {
            self.globals.push(lit);
        }
        Ok(())
    }

    fn emit_mir_function_named(
        &mut self,
        func: &crate::mir::MirFunction,
        name_override: &str,
    ) -> Result<(), String> {
        let sig = self
            .program
            .functions
            .get(&func.name)
            .ok_or_else(|| format!("missing signature for {}", func.name))?;
        let ret_ty = llvm_type(&sig.ret)?;
        let mut params_ir = Vec::new();
        for (idx, param) in sig.params.iter().enumerate() {
            params_ir.push(format!("{} %arg{}", llvm_type(param)?, idx));
        }
        self.output.push_str(&format!(
            "define {} @{}({}) {{\n",
            ret_ty,
            name_override,
            params_ir.join(", ")
        ));
        let mut emitter = FnEmitter::new(
            name_override,
            &self.program.functions,
            &self.program.extern_globals,
            &self.program.types,
            sig.ret.clone(),
        );
        let mut param_names = Vec::new();
        if let Some(ast_func) = self.find_function(&func.name) {
            for param in &ast_func.params {
                param_names.push(param.name.clone());
            }
        } else {
            for idx in 0..sig.params.len() {
                param_names.push(format!("arg{}", idx));
            }
        }
        emitter.emit_prologue(&sig.params, &param_names)?;
        emitter.set_mir_mode(true);
        emitter.set_mir_locals(&func.locals)?;
        emitter.set_mir_expr_types(&func.expr_types);
        let mut block_map = Vec::new();
        let mut block_names = Vec::new();
        block_map.push(0);
        block_names.push("entry".to_string());
        for idx in 1..func.blocks.len() {
            let name = format!("mir_bb{}", idx);
            let block_idx = emitter.add_block(name.clone());
            block_map.push(block_idx);
            block_names.push(name);
        }
        for (idx, block) in func.blocks.iter().enumerate() {
            let block_idx = block_map[idx];
            emitter.switch_to(block_idx);
            let term_is_return = matches!(
                block.term,
                crate::mir::Terminator::Return { .. } | crate::mir::Terminator::ReturnError { .. }
            );
            let mut split_idx = block.stmts.len();
            if term_is_return {
                while split_idx > 0 {
                    if matches!(
                        block.stmts[split_idx - 1],
                        crate::mir::MirStmt::Drop { .. }
                            | crate::mir::MirStmt::DropName { .. }
                            | crate::mir::MirStmt::DeferCall { .. }
                            | crate::mir::MirStmt::ExitScope { .. }
                    ) {
                        split_idx -= 1;
                    } else {
                        break;
                    }
                }
            }
            let (prefix, cleanup) = block.stmts.split_at(split_idx);
            for stmt in prefix {
                emitter.emit_mir_stmt(stmt)?;
                if emitter.current_block_terminated() {
                    break;
                }
            }
            if !emitter.current_block_terminated() {
                if term_is_return {
                    match &block.term {
                        crate::mir::Terminator::Return { value } => {
                            let ret_val = if let Some(expr) = value {
                                Some(emitter.emit_expr(expr)?)
                            } else {
                                None
                            };
                            for stmt in cleanup {
                                emitter.emit_mir_stmt(stmt)?;
                            }
                            emitter.emit_return_value(ret_val)?;
                        }
                        crate::mir::Terminator::ReturnError { err } => {
                            let err_val = emitter.emit_expr(err)?;
                            for stmt in cleanup {
                                emitter.emit_mir_stmt(stmt)?;
                            }
                            emitter.emit_error_return_value(err_val)?;
                        }
                        _ => {}
                    }
                } else {
                    emitter.emit_mir_terminator(&block.term, &block_names)?;
                }
            }
        }
        for block in emitter.blocks {
            self.output.push_str(&format!("{}:\n", block.name));
            for instr in block.instrs {
                self.output.push_str("  ");
                self.output.push_str(&instr);
                self.output.push('\n');
            }
        }
        self.output.push_str("}\n\n");
        for extra in emitter.extra_functions {
            self.output.push_str(&extra);
            self.output.push_str("\n\n");
        }
        for lit in emitter.string_literals {
            self.globals.push(lit);
        }
        Ok(())
    }

    fn find_function(&self, name: &str) -> Option<&Function> {
        for item in &self.program.file.items {
            if let Item::Function(func) = item {
                if func.name == name {
                    return Some(func);
                }
            }
        }
        None
    }

    fn emit_named_types(&mut self) -> Result<(), String> {
        let names = self.program.types.names();
        for name in names {
            let def = self
                .program
                .types
                .get(&name)
                .ok_or_else(|| format!("missing type def for {}", name))?;
            match def {
                TypeDefKind::Struct(def) => {
                    let mut fields = Vec::new();
                    for (_, ty) in &def.fields {
                        fields.push(llvm_type_for_tuple_elem(ty)?);
                    }
                    if def.layout.pack.is_some() || def.layout.bitfield {
                        self.output.push_str(&format!(
                            "%{} = type <{{ {} }}>\n",
                            name,
                            fields.join(", ")
                        ));
                    } else {
                        self.output.push_str(&format!(
                            "%{} = type {{ {} }}\n",
                            name,
                            fields.join(", ")
                        ));
                    }
                }
                TypeDefKind::Enum(_) => {
                    if let TypeDefKind::Enum(def) = def {
                        for (idx, (_, fields)) in def.variants.iter().enumerate() {
                            if fields.is_empty() {
                                continue;
                            }
                            let mut parts = Vec::new();
                            for ty in fields {
                                parts.push(llvm_type_for_tuple_elem(ty)?);
                            }
                            self.output.push_str(&format!(
                                "%{}$payload{} = type {{ {} }}\n",
                                name,
                                idx,
                                parts.join(", ")
                            ));
                        }
                    }
                    self.output
                        .push_str(&format!("%{} = type {{ i32, i8* }}\n", name));
                }
            }
        }
        Ok(())
    }
}

pub(crate) fn llvm_type(ty: &Type) -> Result<String, String> {
    let s = match ty {
        Type::Builtin(BuiltinType::Bool) => "i1".to_string(),
        Type::Builtin(BuiltinType::I32) => "i32".to_string(),
        Type::Builtin(BuiltinType::I64) => "i64".to_string(),
        Type::Builtin(BuiltinType::U32) => "i32".to_string(),
        Type::Builtin(BuiltinType::U64) => "i64".to_string(),
        Type::Builtin(BuiltinType::F32) => "float".to_string(),
        Type::Builtin(BuiltinType::F64) => "double".to_string(),
        Type::Builtin(BuiltinType::Char) => "i32".to_string(),
        Type::Builtin(BuiltinType::Unit) => "void".to_string(),
        Type::Builtin(BuiltinType::String) => "%string".to_string(),
        Type::Builtin(BuiltinType::Error) => "i32".to_string(),
        Type::Builtin(BuiltinType::Bytes) => "%slice".to_string(),
        Type::FnPtr {
            params,
            ret,
            is_variadic,
        } => format!("{}*", llvm_fn_sig(params, ret, *is_variadic)?),
        Type::Ref(inner) | Type::MutRef(inner) => {
            let inner_ty = llvm_type(inner)?;
            if inner_ty == "void" {
                return Err("ref to unit is not supported".to_string());
            }
            format!("{}*", inner_ty)
        }
        Type::Slice(_) => "%slice".to_string(),
        Type::Chan(_) => "%chan*".to_string(),
        Type::Shared(_) => "%shared".to_string(),
        Type::Map(_, _) => "%map".to_string(),
        Type::Interface => "i8*".to_string(),
        Type::Tuple(items) => {
            let mut parts = Vec::new();
            for item in items {
                parts.push(llvm_type_for_tuple_elem(item)?);
            }
            format!("{{ {} }}", parts.join(", "))
        }
        Type::Result(ok, err) => llvm_result_type(ok, err)?,
        Type::Iter(_) => return Err("iter type must be lowered before codegen".to_string()),
        Type::Named(name) => format!("%{}", name),
    };
    Ok(s)
}

pub(crate) fn llvm_call_conv(abi: Option<&str>) -> String {
    let Some(abi) = abi else {
        return String::new();
    };
    let a = abi.to_ascii_lowercase();
    let cc = match a.as_str() {
        "c" | "system" => "",
        "stdcall" => "x86_stdcallcc ",
        "fastcall" => "x86_fastcallcc ",
        "vectorcall" => "x86_vectorcallcc ",
        "thiscall" => "x86_thiscallcc ",
        "win64" => "win64cc ",
        "sysv64" => "x86_64_sysvcc ",
        "aapcs" => "arm_aapcscc ",
        _ => "",
    };
    cc.to_string()
}

fn llvm_fn_sig(params: &[Type], ret: &Type, is_variadic: bool) -> Result<String, String> {
    let mut parts = Vec::new();
    for p in params {
        parts.push(llvm_type(p)?);
    }
    if is_variadic {
        parts.push("...".to_string());
    }
    Ok(format!("{} ({})", llvm_type(ret)?, parts.join(", ")))
}

fn llvm_result_type(ok: &Type, err: &Type) -> Result<String, String> {
    Ok(format!(
        "{{ i8, {}, {} }}",
        llvm_type_for_tuple_elem(ok)?,
        llvm_type_for_tuple_elem(err)?
    ))
}

pub(crate) fn llvm_type_for_tuple_elem(ty: &Type) -> Result<String, String> {
    if *ty == Type::Builtin(BuiltinType::Unit) {
        Ok("i8".to_string())
    } else {
        llvm_type(ty)
    }
}

pub(crate) fn llvm_storage_type(ty: &Type) -> Result<String, String> {
    if *ty == Type::Builtin(BuiltinType::Unit) {
        Ok("i8".to_string())
    } else {
        llvm_type(ty)
    }
}

pub(crate) fn is_float_type(ty: &Type) -> bool {
    matches!(ty, Type::Builtin(BuiltinType::F32 | BuiltinType::F64))
}

pub(crate) fn zero_value(ty: &Type) -> Result<String, String> {
    match ty {
        Type::Builtin(BuiltinType::Bool)
        | Type::Builtin(BuiltinType::I32)
        | Type::Builtin(BuiltinType::I64)
        | Type::Builtin(BuiltinType::U32)
        | Type::Builtin(BuiltinType::U64)
        | Type::Builtin(BuiltinType::Char)
        | Type::Builtin(BuiltinType::Error) => Ok("0".to_string()),
        Type::Builtin(BuiltinType::F32) => Ok("0.0".to_string()),
        Type::Builtin(BuiltinType::F64) => Ok("0.0".to_string()),
        Type::Builtin(BuiltinType::Unit) => Ok("".to_string()),
        Type::Builtin(BuiltinType::String)
        | Type::Builtin(BuiltinType::Bytes)
        | Type::Slice(_)
        | Type::Tuple(_)
        | Type::Result(_, _)
        | Type::Iter(_)
        | Type::Shared(_)
        | Type::Map(_, _)
        | Type::Interface
        | Type::Named(_) => Ok("zeroinitializer".to_string()),
        Type::FnPtr { .. } | Type::Chan(_) | Type::Ref(_) | Type::MutRef(_) => {
            Ok("null".to_string())
        }
    }
}
