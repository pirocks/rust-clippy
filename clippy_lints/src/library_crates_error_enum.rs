use rustc_ast::NodeId;
use rustc_ast::visit::FnKind;
use rustc_hir::{Body, FnDecl, FnRetTy, QPath, Ty, TyKind};
use rustc_lint::{EarlyLintPass, EarlyContext, LintPass, LateLintPass, LateContext};
use rustc_span::def_id::LocalDefId;
use rustc_span::{Span, Symbol};

pub struct LibraryCratesErrorEnum;

impl LintPass for LibraryCratesErrorEnum {
    fn name(&self) -> &'static str {
        todo!()
    }
}


fn is_overly_generic_return_type(ty: &Ty) -> bool{
    match &ty.kind{
        TyKind::Tup(tuple_elems) => {
            tuple_elems.iter().any(is_overly_generic_return_type)
        }
        TyKind::Slice(slice_ty) => {
            is_overly_generic_return_type(slice_ty)
        }
        TyKind::Array(arr_ty, _) => {
            is_overly_generic_return_type(arr_ty)
        }
        TyKind::Ptr(_) => {}
        TyKind::Ref(_, ref_ty) => {
            is_overly_generic_return_type(ref_ty.ty)
        }
        TyKind::BareFn(_) => false,
        TyKind::Never => false,
        TyKind::Path(path) => {
            match path {
                QPath::Resolved(should_be_none, resolved) => {
                    if should_be_none.is_some(){
                        return false
                    }
                    resolved.segments[0].ident.name == Symbol::intern("std") &&
                    resolved.segments[1].ident.name == Symbol::intern("result")
                }
                QPath::TypeRelative(_, _) => {

                }
                QPath::LangItem(_, _, _) => {

                }
            }
            todo!("check for expected types")
        }
        TyKind::OpaqueDef(_, _, _) => {
            false
        }
        TyKind::TraitObject(_, _, _) => {
            false
        }
        TyKind::Typeof(_) => {
            false
        }
        TyKind::Infer => {
            false
        }
        TyKind::Err(_) => {
            false
        }
    }

}

impl LateLintPass for LibraryCratesErrorEnum{


    fn check_fn(&mut self, _: &LateContext<'tcx>, _: rustc_hir::intravisit::FnKind<'tcx>, fn_: &'tcx FnDecl<'tcx>, _: &'tcx Body<'tcx>, _: Span, _: LocalDefId) {
        //We are looking for functions that return anyhow::Result or Result<_, Box<dyn Error>>
        let function_return_type = fn_.output;
        if let FnRetTy::Return(return_type) = function_return_type {
            is_overly_generic_return_type(&return_type)
        }
    }
}