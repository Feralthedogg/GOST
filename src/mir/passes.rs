use std::collections::{HashMap, VecDeque};

use crate::frontend::ast::{Block, Expr, ExprKind, Stmt};
use crate::sema::types::{Type, TypeClass, TypeDefKind, TypeDefs};

use super::{BasicBlock, CleanupItem, MirFunction, MirStmt, ScopeId, Terminator};

pub fn build_cleanup_chains(func: &mut MirFunction) -> Result<(), String> {
    if func.blocks.is_empty() {
        return Ok(());
    }
    let block_count = func.blocks.len();
    if func.block_scopes.len() != block_count || func.block_exit_scopes.len() != block_count {
        return Err("missing scope metadata for cleanup chains".to_string());
    }

    let orig_len = func.blocks.len();
    for bb in 0..orig_len {
        let mut new_stmts = Vec::new();
        for stmt in func.blocks[bb].stmts.drain(..) {
            match stmt {
                MirStmt::ExitScope { scope } => {
                    let items = func
                        .scopes
                        .get(scope)
                        .ok_or_else(|| "missing scope frame".to_string())?
                        .items
                        .clone();
                    emit_cleanup_items_from(&items, &mut new_stmts);
                    new_stmts.push(MirStmt::ExitScope { scope });
                }
                other => new_stmts.push(other),
            }
        }
        func.blocks[bb].stmts = new_stmts;
    }

    let mut cleanup_cache: HashMap<(Option<ScopeId>, Option<ScopeId>, usize), usize> =
        HashMap::new();

    for bb in 0..orig_len {
        let from_scope = func.block_exit_scopes.get(bb).copied().flatten();
        let term = func.blocks[bb].term.clone();
        match term {
            Terminator::Return { .. } | Terminator::ReturnError { .. } => {
                let scopes = scope_chain(func, from_scope, None)?;
                for scope in scopes {
                    let items = func
                        .scopes
                        .get(scope)
                        .ok_or_else(|| "missing scope frame".to_string())?
                        .items
                        .clone();
                    emit_cleanup_items_from(&items, &mut func.blocks[bb].stmts);
                    func.blocks[bb]
                        .stmts
                        .push(MirStmt::ExitScope { scope });
                }
            }
            Terminator::Goto(target) => {
                let to_scope = func.block_scopes.get(target).copied().flatten();
                let new_target = edge_cleanup_target(
                    func,
                    &mut cleanup_cache,
                    from_scope,
                    to_scope,
                    target,
                )?;
                func.blocks[bb].term = Terminator::Goto(new_target);
            }
            Terminator::If { cond, then_bb, else_bb } => {
                let then_scope = func.block_scopes.get(then_bb).copied().flatten();
                let else_scope = func.block_scopes.get(else_bb).copied().flatten();
                let then_target = edge_cleanup_target(
                    func,
                    &mut cleanup_cache,
                    from_scope,
                    then_scope,
                    then_bb,
                )?;
                let else_target = edge_cleanup_target(
                    func,
                    &mut cleanup_cache,
                    from_scope,
                    else_scope,
                    else_bb,
                )?;
                func.blocks[bb].term = Terminator::If {
                    cond,
                    then_bb: then_target,
                    else_bb: else_target,
                };
            }
            Terminator::Match { scrutinee, arms, default } => {
                let mut new_arms = Vec::new();
                for (pat, target) in arms {
                    let to_scope = func.block_scopes.get(target).copied().flatten();
                    let target = edge_cleanup_target(
                        func,
                        &mut cleanup_cache,
                        from_scope,
                        to_scope,
                        target,
                    )?;
                    new_arms.push((pat, target));
                }
                let new_default = if let Some(target) = default {
                    let to_scope = func.block_scopes.get(target).copied().flatten();
                    Some(edge_cleanup_target(
                        func,
                        &mut cleanup_cache,
                        from_scope,
                        to_scope,
                        target,
                    )?)
                } else {
                    None
                };
                func.blocks[bb].term = Terminator::Match {
                    scrutinee,
                    arms: new_arms,
                    default: new_default,
                };
            }
        }
    }
    Ok(())
}

fn edge_cleanup_target(
    func: &mut MirFunction,
    cache: &mut HashMap<(Option<ScopeId>, Option<ScopeId>, usize), usize>,
    from_scope: Option<ScopeId>,
    to_scope: Option<ScopeId>,
    target: usize,
) -> Result<usize, String> {
    if from_scope == to_scope {
        return Ok(target);
    }
    if let Some(entry) = cache.get(&(from_scope, to_scope, target)) {
        return Ok(*entry);
    }
    let scopes = scope_chain(func, from_scope, to_scope)?;
    if scopes.is_empty() {
        cache.insert((from_scope, to_scope, target), target);
        return Ok(target);
    }
    let mut next_target = target;
    for scope in scopes.iter().rev() {
        let mut stmts = Vec::new();
        let items = func
            .scopes
            .get(*scope)
            .ok_or_else(|| "missing scope frame".to_string())?
            .items
            .clone();
        emit_cleanup_items_from(&items, &mut stmts);
        stmts.push(MirStmt::ExitScope { scope: *scope });
        let bb = func.blocks.len();
        func.blocks.push(BasicBlock {
            stmts,
            term: Terminator::Goto(next_target),
        });
        let depth = func
            .scopes
            .get(*scope)
            .map(|frame| frame.depth)
            .unwrap_or(0);
        func.block_depths.push(depth);
        func.block_scopes.push(Some(*scope));
        let parent = func.scopes.get(*scope).and_then(|frame| frame.parent);
        func.block_exit_scopes.push(parent);
        next_target = bb;
    }
    cache.insert((from_scope, to_scope, target), next_target);
    Ok(next_target)
}

fn scope_chain(
    func: &MirFunction,
    from_scope: Option<ScopeId>,
    to_scope: Option<ScopeId>,
) -> Result<Vec<ScopeId>, String> {
    if from_scope == to_scope {
        return Ok(Vec::new());
    }
    let mut out = Vec::new();
    let mut cursor = from_scope;
    while cursor != to_scope {
        let scope_id = cursor.ok_or_else(|| "scope mismatch across control-flow edge".to_string())?;
        out.push(scope_id);
        cursor = func
            .scopes
            .get(scope_id)
            .ok_or_else(|| "missing scope frame".to_string())?
            .parent;
    }
    Ok(out)
}

fn emit_cleanup_items_from(items: &[CleanupItem], out: &mut Vec<MirStmt>) {
    let mut defers = Vec::new();
    let mut drops = Vec::new();
    for item in items {
        match item {
            CleanupItem::DeferCall { call } => defers.push(call.clone()),
            CleanupItem::DropLocal { local } => {
                drops.push(MirStmt::Drop { local: *local });
            }
            CleanupItem::DropName { name, ty } => drops.push(MirStmt::DropName {
                name: name.clone(),
                ty: ty.clone(),
            }),
        }
    }
    for call in defers.iter().rev() {
        out.push(MirStmt::DeferCall { call: call.clone() });
    }
    for drop in drops.iter().rev() {
        out.push(drop.clone());
    }
}

pub fn insert_drops(func: &mut MirFunction, defs: &TypeDefs) {
    let mut drop_locals = Vec::new();
    for (idx, local) in func.locals.iter().enumerate() {
        if needs_drop(defs, &local.ty) {
            drop_locals.push(idx);
        }
    }
    if drop_locals.is_empty() {
        return;
    }
    for block in &mut func.blocks {
        let is_return = matches!(
            block.term,
            Terminator::Return { .. } | Terminator::ReturnError { .. }
        );
        if !is_return {
            continue;
        }
        for &local in drop_locals.iter().rev() {
            block.stmts.push(MirStmt::Drop { local });
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum LinState {
    Uninit,
    Alive,
    Moved,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ExprCtx {
    Value,
    Place,
}

pub fn linear_check(func: &mut MirFunction) -> Result<(), String> {
    let mut name_to_local = HashMap::new();
    let mut local_names = vec![None; func.locals.len()];
    let mut linear_mask = vec![false; func.locals.len()];
    for (idx, local) in func.locals.iter().enumerate() {
        if local.class == TypeClass::Linear {
            linear_mask[idx] = true;
            if let Some(name) = &local.name {
                name_to_local.insert(name.clone(), idx);
                local_names[idx] = Some(name.clone());
            }
        }
    }
    if !linear_mask.iter().any(|v| *v) {
        return Ok(());
    }
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); func.blocks.len()];
    for (idx, block) in func.blocks.iter().enumerate() {
        for succ in successors(&block.term) {
            preds[succ].push(idx);
        }
    }
    let mut in_state = vec![vec![LinState::Uninit; func.locals.len()]; func.blocks.len()];
    let mut out_state = vec![vec![LinState::Uninit; func.locals.len()]; func.blocks.len()];
    let mut worklist = VecDeque::new();
    if !func.blocks.is_empty() {
        let entry = func.entry;
        let param_limit = std::cmp::min(func.param_count, linear_mask.len());
        for idx in 0..param_limit {
            if linear_mask[idx] {
                in_state[entry][idx] = LinState::Alive;
            }
        }
        worklist.push_back(entry);
    }
    while let Some(bb) = worklist.pop_front() {
        let mut state = if bb == func.entry {
            in_state[bb].clone()
        } else if preds[bb].is_empty() {
            in_state[bb].clone()
        } else {
            join_states(
                preds[bb].iter().map(|p| &out_state[*p]),
                &linear_mask,
                &local_names,
            )?
        };
        if state != in_state[bb] {
            in_state[bb] = state.clone();
        }
        apply_block_effects(
            &func.blocks[bb],
            &mut state,
            &name_to_local,
            &linear_mask,
            &local_names,
        )
        .map_err(|e| format!("{}: {}", func.name, e))?;
        if state != out_state[bb] {
            out_state[bb] = state;
            for succ in successors(&func.blocks[bb].term) {
                worklist.push_back(succ);
            }
        }
    }
    for (bb, block) in func.blocks.iter_mut().enumerate() {
        let mut state = in_state[bb].clone();
        let mut new_stmts = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                MirStmt::Drop { local } => {
                    if !linear_mask[*local] {
                        new_stmts.push(stmt.clone());
                        continue;
                    }
                    if state[*local] == LinState::Alive {
                        state[*local] = LinState::Moved;
                        new_stmts.push(stmt.clone());
                    }
                    continue;
                }
                MirStmt::DropName { name, .. } => {
                    if let Some(local) = name_to_local.get(name).copied() {
                        if state[local] == LinState::Alive {
                            state[local] = LinState::Moved;
                            new_stmts.push(stmt.clone());
                        }
                        continue;
                    }
                    new_stmts.push(stmt.clone());
                    continue;
                }
                _ => {}
            }
            apply_stmt_effects(
                stmt,
                &mut state,
                &name_to_local,
                &linear_mask,
                &local_names,
            )
            .map_err(|e| format!("{}: {}", func.name, e))?;
            new_stmts.push(stmt.clone());
        }
        block.stmts = new_stmts;
    }
    Ok(())
}

pub fn verify_mir_strict(func: &MirFunction) -> Result<(), String> {
    let block_count = func.blocks.len();
    if block_count == 0 {
        return Err(format!(
            "MIR-only violation: function {} has no basic blocks",
            func.name
        ));
    }
    if func.entry >= block_count {
        return Err(format!(
            "MIR-only violation: function {} entry block {} out of range",
            func.name, func.entry
        ));
    }
    if func.block_depths.len() != block_count {
        return Err(format!(
            "MIR-only violation: function {} block_depths length mismatch",
            func.name
        ));
    }
    if func.block_scopes.len() != block_count {
        return Err(format!(
            "MIR-only violation: function {} block_scopes length mismatch",
            func.name
        ));
    }
    if func.block_exit_scopes.len() != block_count {
        return Err(format!(
            "MIR-only violation: function {} block_exit_scopes length mismatch",
            func.name
        ));
    }

    for (scope_id, scope) in func.scopes.iter().enumerate() {
        if scope.depth < 0 {
            return Err(format!(
                "MIR-only violation: function {} scope {} has negative depth",
                func.name, scope_id
            ));
        }
        if let Some(parent) = scope.parent {
            if parent >= func.scopes.len() {
                return Err(format!(
                    "MIR-only violation: function {} scope {} has invalid parent {}",
                    func.name, scope_id, parent
                ));
            }
            let parent_depth = func.scopes[parent].depth;
            if parent_depth >= scope.depth {
                return Err(format!(
                    "MIR-only violation: function {} scope {} depth {} not greater than parent depth {}",
                    func.name, scope_id, scope.depth, parent_depth
                ));
            }
        }
    }

    for (bb_idx, block) in func.blocks.iter().enumerate() {
        let depth = func.block_depths[bb_idx];
        if depth < 0 {
            return Err(format!(
                "MIR-only violation: function {} bb {} has negative depth",
                func.name, bb_idx
            ));
        }
        if let Some(scope_id) = func.block_scopes[bb_idx] {
            if scope_id >= func.scopes.len() {
                return Err(format!(
                    "MIR-only violation: function {} bb {} has invalid scope {}",
                    func.name, bb_idx, scope_id
                ));
            }
        }
        if let Some(scope_id) = func.block_exit_scopes[bb_idx] {
            if scope_id >= func.scopes.len() {
                return Err(format!(
                    "MIR-only violation: function {} bb {} has invalid exit scope {}",
                    func.name, bb_idx, scope_id
                ));
            }
        }

        let check_target = |target: usize, func_name: &str, bb_idx: usize| -> Result<(), String> {
            if target >= block_count {
                return Err(format!(
                    "MIR-only violation: function {} bb {} has invalid target bb {}",
                    func_name, bb_idx, target
                ));
            }
            Ok(())
        };
        for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
            match stmt {
                MirStmt::Select { .. } => {
                    return Err(format!(
                        "MIR-only violation: MirStmt::Select remains in {} (bb {}, stmt {})",
                        func.name, bb_idx, stmt_idx
                    ));
                }
                MirStmt::ForIn { .. } => {
                    return Err(format!(
                        "MIR-only violation: MirStmt::ForIn remains in {} (bb {}, stmt {})",
                        func.name, bb_idx, stmt_idx
                    ));
                }
                _ => {}
            }
        }
        match &block.term {
            Terminator::Goto(target) => {
                check_target(*target, &func.name, bb_idx)?;
            }
            Terminator::If { then_bb, else_bb, .. } => {
                check_target(*then_bb, &func.name, bb_idx)?;
                check_target(*else_bb, &func.name, bb_idx)?;
            }
            Terminator::Match { arms, default, .. } => {
                for (_, target) in arms {
                    check_target(*target, &func.name, bb_idx)?;
                }
                if let Some(target) = default {
                    check_target(*target, &func.name, bb_idx)?;
                }
            }
            Terminator::Return { .. } | Terminator::ReturnError { .. } => {}
        }
    }
    Ok(())
}

fn successors(term: &Terminator) -> Vec<usize> {
    match term {
        Terminator::Goto(target) => vec![*target],
        Terminator::If { then_bb, else_bb, .. } => vec![*then_bb, *else_bb],
        Terminator::Match { arms, default, .. } => {
            let mut out = Vec::new();
            for (_, bb) in arms {
                out.push(*bb);
            }
            if let Some(bb) = default {
                out.push(*bb);
            }
            out
        }
        Terminator::Return { .. } | Terminator::ReturnError { .. } => Vec::new(),
    }
}

fn join_states<'a, I>(
    states: I,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<Vec<LinState>, String>
where
    I: Iterator<Item = &'a Vec<LinState>>,
{
    let mut joined: Option<Vec<LinState>> = None;
    for state in states {
        match &mut joined {
            None => joined = Some(state.clone()),
            Some(accum) => {
                for (idx, item) in accum.iter_mut().enumerate() {
                    if !linear_mask[idx] {
                        continue;
                    }
                    let next = state[idx];
                    if *item == LinState::Uninit {
                        *item = next;
                        continue;
                    }
                    if next == LinState::Uninit {
                        continue;
                    }
                    if *item == next {
                        continue;
                    }
                    let name = local_names[idx]
                        .as_deref()
                        .unwrap_or("<linear>");
                    return Err(format!(
                        "ownership differs across control-flow paths for {}",
                        name
                    ));
                }
            }
        }
    }
    Ok(joined.unwrap_or_else(|| vec![LinState::Uninit; linear_mask.len()]))
}

fn apply_block_effects(
    block: &super::BasicBlock,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    for stmt in &block.stmts {
        match stmt {
            MirStmt::Drop { local } => {
                if linear_mask[*local] && state[*local] == LinState::Alive {
                    state[*local] = LinState::Moved;
                }
                continue;
            }
            MirStmt::DropName { name, .. } => {
                if let Some(local) = name_to_local.get(name).copied() {
                    if linear_mask[local] && state[local] == LinState::Alive {
                        state[local] = LinState::Moved;
                    }
                }
                continue;
            }
            _ => {}
        }
        apply_stmt_effects(stmt, state, name_to_local, linear_mask, local_names)?;
    }
    apply_term_effects(&block.term, state, name_to_local, linear_mask, local_names)
}

fn apply_stmt_effects(
    stmt: &MirStmt,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    match stmt {
        MirStmt::Let { local, init } => {
            apply_expr_moves(
                init,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            if linear_mask[*local] {
                state[*local] = LinState::Alive;
            }
            Ok(())
        }
        MirStmt::Assign { target, value } => {
            apply_expr_moves(
                value,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            apply_expr_moves(
                target,
                ExprCtx::Place,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            // Re-initialize moved linear locals on assignment to an identifier.
            if let ExprKind::Ident(name) = &target.kind {
                if let Some(local) = name_to_local.get(name).copied() {
                    if linear_mask[local] {
                        state[local] = LinState::Alive;
                    }
                }
            }
            Ok(())
        }
        MirStmt::Expr { expr } => apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        MirStmt::DeferCall { call } => apply_expr_moves(
            call,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        MirStmt::Eval { expr, out } => {
            apply_expr_moves(
                expr,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            for local in out {
                if linear_mask[*local] {
                    state[*local] = LinState::Alive;
                }
            }
            Ok(())
        }
        MirStmt::ForIn { iter, body, .. } => {
            apply_expr_moves(
                iter,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            apply_block_moves(body, state, name_to_local, linear_mask, local_names)
        }
        MirStmt::Select { arms } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        apply_expr_moves(
                            chan,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                        apply_expr_moves(
                            value,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. }
                    | crate::frontend::ast::SelectArmKind::After { ms: chan } => {
                        apply_expr_moves(
                            chan,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
            }
            Ok(())
        }
        MirStmt::Go { expr } => apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        MirStmt::EnterScope { .. }
        | MirStmt::ExitScope { .. }
        | MirStmt::Drop { .. }
        | MirStmt::DropName { .. } => Ok(()),
    }
}

fn apply_term_effects(
    term: &Terminator,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    match term {
        Terminator::If { cond, .. } => apply_expr_moves(
            cond,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Terminator::Match { scrutinee, .. } => apply_expr_moves(
            scrutinee,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Terminator::Return { value } => {
            if let Some(expr) = value {
                apply_expr_moves(
                    expr,
                    ExprCtx::Value,
                    state,
                    name_to_local,
                    linear_mask,
                    local_names,
                )?;
            }
            Ok(())
        }
        Terminator::ReturnError { err } => apply_expr_moves(
            err,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Terminator::Goto(_) => Ok(()),
    }
}

fn apply_ast_stmt_moves(
    stmt: &Stmt,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    match stmt {
        Stmt::Let { init, .. } => apply_expr_moves(
            init,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Stmt::Assign { target, value, .. } => {
            apply_expr_moves(
                value,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            apply_expr_moves(
                target,
                ExprCtx::Place,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )
        }
        Stmt::Expr { expr, .. } => apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Stmt::Return { expr, .. } => {
            if let Some(expr) = expr {
                apply_expr_moves(
                    expr,
                    ExprCtx::Value,
                    state,
                    name_to_local,
                    linear_mask,
                    local_names,
                )?;
            }
            Ok(())
        }
        Stmt::ForIn { iter, body, .. } => {
            apply_expr_moves(
                iter,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            apply_block_moves(body, state, name_to_local, linear_mask, local_names)
        }
        Stmt::While { cond, body, .. } => {
            apply_expr_moves(
                cond,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            apply_block_moves(body, state, name_to_local, linear_mask, local_names)
        }
        Stmt::Loop { body, .. } => apply_block_moves(body, state, name_to_local, linear_mask, local_names),
        Stmt::Select { arms, .. } => {
            for arm in arms {
                match &arm.kind {
                    crate::frontend::ast::SelectArmKind::Send { chan, value } => {
                        apply_expr_moves(
                            chan,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                        apply_expr_moves(
                            value,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Recv { chan, .. }
                    | crate::frontend::ast::SelectArmKind::After { ms: chan } => {
                        apply_expr_moves(
                            chan,
                            ExprCtx::Value,
                            state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                    }
                    crate::frontend::ast::SelectArmKind::Default => {}
                }
            }
            Ok(())
        }
        Stmt::Go { expr, .. } => apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Stmt::Defer { expr, .. } => apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        ),
        Stmt::Break { .. } | Stmt::Continue { .. } => Ok(()),
    }
}

fn apply_block_moves(
    block: &Block,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    for stmt in &block.stmts {
        apply_ast_stmt_moves(stmt, state, name_to_local, linear_mask, local_names)?;
    }
    if let Some(expr) = &block.tail {
        apply_expr_moves(
            expr,
            ExprCtx::Value,
            state,
            name_to_local,
            linear_mask,
            local_names,
        )?;
    }
    Ok(())
}

fn apply_expr_moves(
    expr: &Expr,
    ctx: ExprCtx,
    state: &mut [LinState],
    name_to_local: &HashMap<String, usize>,
    linear_mask: &[bool],
    local_names: &[Option<String>],
) -> Result<(), String> {
    match &expr.kind {
        ExprKind::Ident(name) => {
            if let Some(local) = name_to_local.get(name).copied() {
                if !linear_mask[local] {
                    return Ok(());
                }
                if ctx == ExprCtx::Place {
                    return Ok(());
                }
                if state[local] != LinState::Alive {
                    let name = local_names[local]
                        .as_deref()
                        .unwrap_or("<linear>");
                    return Err(format!("use after move: {}", name));
                }
                state[local] = LinState::Moved;
            }
            Ok(())
        }
        ExprKind::Borrow { expr: inner, .. } => {
            apply_expr_moves(inner, ExprCtx::Place, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Field { base, .. } => {
            apply_expr_moves(base, ExprCtx::Place, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Index { base, index } => {
            apply_expr_moves(base, ExprCtx::Place, state, name_to_local, linear_mask, local_names)?;
            apply_expr_moves(index, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Deref { expr: inner } => {
            apply_expr_moves(inner, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::StructLit { fields, .. } => {
            for (_, expr) in fields {
                apply_expr_moves(expr, ExprCtx::Value, state, name_to_local, linear_mask, local_names)?;
            }
            Ok(())
        }
        ExprKind::Unary { expr: inner, .. } => {
            apply_expr_moves(inner, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Binary { left, right, .. } => {
            apply_expr_moves(left, ExprCtx::Value, state, name_to_local, linear_mask, local_names)?;
            apply_expr_moves(
                right,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )
        }
        ExprKind::Tuple(items) => {
            for item in items {
                apply_expr_moves(
                    item,
                    ExprCtx::Value,
                    state,
                    name_to_local,
                    linear_mask,
                    local_names,
                )?;
            }
            Ok(())
        }
        ExprKind::Call { callee, args, .. } => {
            let mut arg_ctx = ExprCtx::Value;
            if let ExprKind::Ident(name) = &callee.kind {
                if name == "__gost_chan_can_send" || name == "__gost_chan_can_recv" {
                    arg_ctx = ExprCtx::Place;
                } else if name == "__gost_select_wait" {
                    return Ok(());
                }
            }
            for arg in args {
                apply_expr_moves(
                    arg,
                    arg_ctx,
                    state,
                    name_to_local,
                    linear_mask,
                    local_names,
                )?;
            }
            Ok(())
        }
        ExprKind::Block(block) | ExprKind::UnsafeBlock(block) => {
            apply_block_moves(block, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::If { cond, then_block, else_block } => {
            apply_expr_moves(cond, ExprCtx::Value, state, name_to_local, linear_mask, local_names)?;
            let mut then_state = state.to_vec();
            apply_block_moves(then_block, &mut then_state, name_to_local, linear_mask, local_names)?;
            let mut else_state = state.to_vec();
            if let Some(block) = else_block {
                apply_block_moves(block, &mut else_state, name_to_local, linear_mask, local_names)?;
            }
            let joined = join_states(
                vec![&then_state, &else_state].into_iter(),
                linear_mask,
                local_names,
            )?;
            state.copy_from_slice(&joined);
            Ok(())
        }
        ExprKind::Match { scrutinee, arms } => {
            apply_expr_moves(
                scrutinee,
                ExprCtx::Value,
                state,
                name_to_local,
                linear_mask,
                local_names,
            )?;
            let mut arm_states = Vec::new();
            for arm in arms {
                let mut arm_state = state.to_vec();
                match &arm.body {
                    crate::frontend::ast::BlockOrExpr::Block(block) => {
                        apply_block_moves(block, &mut arm_state, name_to_local, linear_mask, local_names)?;
                    }
                    crate::frontend::ast::BlockOrExpr::Expr(expr) => {
                        apply_expr_moves(
                            expr,
                            ExprCtx::Value,
                            &mut arm_state,
                            name_to_local,
                            linear_mask,
                            local_names,
                        )?;
                    }
                }
                arm_states.push(arm_state);
            }
            let mut refs = Vec::new();
            for state_ref in &arm_states {
                refs.push(state_ref);
            }
            let joined = join_states(refs.into_iter(), linear_mask, local_names)?;
            state.copy_from_slice(&joined);
            Ok(())
        }
        ExprKind::Try { expr: inner } => {
            apply_expr_moves(inner, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Send { chan, value } => {
            apply_expr_moves(chan, ExprCtx::Place, state, name_to_local, linear_mask, local_names)?;
            apply_expr_moves(value, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Recv { chan } | ExprKind::Close { chan } => {
            apply_expr_moves(chan, ExprCtx::Place, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::After { ms } => {
            apply_expr_moves(ms, ExprCtx::Value, state, name_to_local, linear_mask, local_names)
        }
        ExprKind::Bool(_)
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Char(_)
        | ExprKind::String(_)
        | ExprKind::Nil => Ok(()),
    }
}

fn needs_drop(defs: &TypeDefs, ty: &Type) -> bool {
    match ty {
        Type::Builtin(crate::sema::types::BuiltinType::Bytes) => true,
        Type::Builtin(_) => false,
        Type::Slice(_) | Type::Map(_, _) | Type::Chan(_) | Type::Shared(_) => true,
        Type::Tuple(items) => items.iter().any(|item| needs_drop(defs, item)),
        Type::Result(ok, err) => needs_drop(defs, ok) || needs_drop(defs, err),
        Type::Named(name) => match defs.get(name) {
            Some(TypeDefKind::Struct(def)) => def
                .fields
                .iter()
                .any(|(_, field_ty)| needs_drop(defs, field_ty)),
            Some(TypeDefKind::Enum(def)) => def
                .variants
                .iter()
                .any(|(_, fields)| fields.iter().any(|field_ty| needs_drop(defs, field_ty))),
            None => false,
        },
        _ => false,
    }
}
