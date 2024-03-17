use std::collections::HashMap;
use rustc_hir::{Body, Expr, Ty};
use crate::consts::Constant;

pub struct SymbolId(usize);



pub struct SymbolicValue<'tcx,'hir> {
    kind: SymbolicValueKind<'tcx>,
    ty: &'tcx Ty<'hir>,
}

pub enum SymbolicValueKind<'tcx>{
    Constant(Constant<'tcx>),
    Symbol(SymbolId),
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

    //evaluate with a function
    pub fn evaluate_body(function: rustc_middle::mir::Body, args: Vec<SymbolicValue<'tcx, 'hir>>) -> EvaluationResult<'tcx, 'hir> {

        assert_eq!(function.params.len(), args.len());
        let symbols = HashMap::new();
        function.params.iter().zip(args).for_each(|(param, arg)|{
            param.pat.kind
            symbols.insert(param.id, arg);
        }
        let context = EvaluationContext{ symbols, max_evals: Self::MAX_EVALS };
        EvaluationResult {
            value,
            side_effects,
        }
    }

    pub fn evaluate_expr(&mut self, expr: Expr) -> EvaluationResult<'tcx, 'hir> {
        let mut side_effects = vec![];
        EvaluationResult {
            value,
            side_effects,
        }
    }
}

pub struct EvaluationResult<'tcx, 'hir>{
    value: SymbolicValue<'tcx, 'hir>,
    side_effects: Vec<SideEffect<'tcx,'hir>>
}

pub enum SideEffect<'tcx,'hir>{
    Write(SymbolicValue<'tcx, 'hir>),
}

