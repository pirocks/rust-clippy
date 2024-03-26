use std::collections::HashMap;
use rustc_hir::{Body, Expr, ExprKind, Ty};
use rustc_middle::mir::{Statement, StatementKind};
use crate::consts::Constant;

pub struct SymbolId(usize);



pub struct SymbolicValue<'tcx,'hir> {
    kind: SymbolicValueKind<'tcx>,
    ty: &'tcx Ty<'hir>,
}

pub enum SymbolicValueKind<'tcx>{
    Constant(Constant<'tcx>),
    Symbol(SymbolId),
    UnImplemented
}

pub struct EvaluationContext<'tcx,'hir> {
    symbols: HashMap<SymbolId, SymbolicValue<'tcx,'hir>>,
    max_evals: usize,
}

impl EvaluationContext {
    pub const MAX_EVALS: usize = 1000;

    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            max_evals: Self::MAX_EVALS,
        }
    }

    pub fn evaluate_function(function: rustc_middle::mir::)


    //evaluate with a function
    pub fn evaluate_body(function: rustc_middle::mir::Body, args: Vec<SymbolicValue<'tcx, 'hir>>) -> EvaluationResult<'tcx, 'hir> {

        assert_eq!(function.params.len(), args.len());
        let mut symbols = HashMap::new();
        function.local_decls.iter().zip(args).for_each(|(local_decl_id, arg)|{
            symbols.insert(param.id, arg);
        });
        let context = EvaluationContext{ symbols, max_evals: Self::MAX_EVALS };
        EvaluationResult {
            value,
            side_effects,
        }
    }

    pub fn evaluate_stmt(statement: Statement) {
        match statement.kind {
            StatementKind::Assign(_) => {}
            StatementKind::FakeRead(_) => {}
            StatementKind::SetDiscriminant { .. } => {}
            StatementKind::Deinit(_) => {}
            StatementKind::StorageLive(_) => {}
            StatementKind::StorageDead(_) => {}
            StatementKind::Retag(_, _) => {}
            StatementKind::PlaceMention(_) => {}
            StatementKind::AscribeUserType(_, _) => {}
            StatementKind::Coverage(_) => {}
            StatementKind::Intrinsic(_) => {}
            StatementKind::ConstEvalCounter => {}
            StatementKind::Nop => {}
        }
    }

/*    pub fn evaluate_expr(&mut self, expr: Expr) -> EvaluationResult<'tcx, 'hir> {
        let mut side_effects = vec![];
        match expr.kind {
            ExprKind::ConstBlock(_) => {}
            ExprKind::Array(_) => {}
            ExprKind::Call(_, _) => {}
            ExprKind::MethodCall(_, _, _, _) => {}
            ExprKind::Tup(_) => {}
            ExprKind::Binary(_, _, _) => {}
            ExprKind::Unary(_, _) => {}
            ExprKind::Lit(_) => {}
            ExprKind::Cast(_, _) => {}
            ExprKind::Type(_, _) => {}
            ExprKind::DropTemps(_) => {}
            ExprKind::Let(_) => {}
            ExprKind::If(_, _, _) => {}
            ExprKind::Loop(_, _, _, _) => {}
            ExprKind::Match(_, _, _) => {}
            ExprKind::Closure(_) => {}
            ExprKind::Block(_, _) => {}
            ExprKind::Assign(_, _, _) => {}
            ExprKind::AssignOp(_, _, _) => {}
            ExprKind::Field(_, _) => {}
            ExprKind::Index(_, _, _) => {}
            ExprKind::Path(_) => {}
            ExprKind::AddrOf(_, _, _) => {}
            ExprKind::Break(_, _) => {}
            ExprKind::Continue(_) => {}
            ExprKind::Ret(_) => {}
            ExprKind::Become(_) => {}
            ExprKind::InlineAsm(_) => {}
            ExprKind::OffsetOf(_, _) => {}
            ExprKind::Struct(_, _, _) => {}
            ExprKind::Repeat(_, _) => {}
            ExprKind::Yield(_, _) => {}
            ExprKind::Err(_) => {}
        }
        EvaluationResult {
            value,
            side_effects,
        }
    }
*/}

pub struct EvaluationResult<'tcx, 'hir>{
    value: SymbolicValue<'tcx, 'hir>,
    side_effects: Vec<SideEffect<'tcx,'hir>>
}

pub enum SideEffect<'tcx,'hir>{
    Write(SymbolicValue<'tcx, 'hir>),
}

