use clippy_utils::{match_path};
use clippy_utils::ty::is_type_lang_item;
use rustc_hir::{Body, FnDecl, FnRetTy, GenericArg, LangItem, QPath, Ty, TyKind};
use rustc_lint::{LateLintPass, LateContext};
use rustc_session::declare_lint_pass;
use rustc_span::def_id::{LocalDefId};
use rustc_span::Span;

declare_clippy_lint! {
    /// ### What it does
    ///
    ///
    /// ### Why is this bad?
    ///
    ///
    /// ### Known problems
    ///
    ///
    /// ### Example
    /// Before:
    /// ```no_run
    ///
    /// ```
    ///
    /// After:
    /// ```no_run
    ///
    /// ```
    #[clippy::version = "1.48.0"]
    pub LIBRARY_CRATES_ERROR_ENUM,
    restriction,
    "library crates that use overly general error types, like Result<(),Box<dyn Error>> or Result<(),String>"
}

declare_lint_pass!(LibraryCratesErrorEnum=> [LIBRARY_CRATES_ERROR_ENUM]);

fn is_overly_generic_error_type(cx: &LateContext, arg: &GenericArg) -> bool{
    match arg{
        GenericArg::Lifetime(_) => {}
        GenericArg::Type(ty) => {
            if is_type_lang_item(cx, (*ty).clone(), LangItem::String){
                return true
            }
            if is_type_lang_item(cx, (*ty).clone(), LangItem::OwnedBox){
                dbg!(ty);
                todo!()
            }
        }
        GenericArg::Const(_) => {}
        GenericArg::Infer(_) => {}
    }
    false
}

fn is_overly_generic_return_type(cx: &LateContext<'_>, ty: &Ty<'_>) -> bool{
    match &ty.kind{
        TyKind::Tup(tuple_elems) => {
            tuple_elems.iter().any(|ty|is_overly_generic_return_type(cx,ty))
        }
        TyKind::Slice(slice_ty) => {
            is_overly_generic_return_type(cx, slice_ty)
        }
        TyKind::Array(arr_ty, _) => {
            is_overly_generic_return_type(cx, arr_ty)
        }
        TyKind::Ptr(_) => {
            false
        }
        TyKind::Ref(_, ref_ty) => {
            is_overly_generic_return_type(cx, ref_ty.ty)
        }
        TyKind::BareFn(_) => false,
        TyKind::Never => false,
        TyKind::Path(path) => {
            match path {
                QPath::Resolved(should_be_none, resolved) => {
                    if should_be_none.is_some(){
                        return false
                    }
                    if match_path(resolved, &["core", "result", "Result"]) && resolved.segments.len() == 3 {
                        if let Some(args) = resolved.segments[2].args{
                            if args.args.len() == 2{
                                return is_overly_generic_error_type(cx, &args.args[1])
                            }else {
                                false
                            }
                        }else {
                            false
                        }
                    }else {
                        false
                    }
                }
                QPath::TypeRelative(_, _) => {
                    false
                }
                QPath::LangItem(_, _) => {
                    false
                }
            }
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
        TyKind::InferDelegation(_, _) => {
            false
        }
    }
}

impl<'tcx> LateLintPass<'tcx> for LibraryCratesErrorEnum{

    fn check_fn(&mut self,
                cx: &LateContext<'tcx>,
                _: rustc_hir::intravisit::FnKind<'tcx>,
                fn_: &'tcx FnDecl<'tcx>,
                _: &'tcx Body<'tcx>,
                _: Span,
                _: LocalDefId) {
        //We are looking for functions that return anyhow::Result or
        // Result<_, Box<dyn Error>> or Result<_, String>
        let function_return_type = fn_.output;
        if let FnRetTy::Return(return_type) = function_return_type {
            if is_overly_generic_return_type(cx, &return_type){
                todo!()
            }
        }
    }
}