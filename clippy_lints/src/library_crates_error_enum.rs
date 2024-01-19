use clippy_utils::ty::{is_type_lang_item};
use rustc_hir::{Body, FnDecl, LangItem};
use rustc_lint::{LateLintPass, LateContext};
use rustc_middle::ty::{ExistentialPredicate, Ty, TyKind};
use rustc_session::{declare_lint_pass};
use rustc_span::def_id::{LocalDefId};
use rustc_span::{Span};
use clippy_utils::diagnostics::{span_lint_and_note};
use clippy_utils::{get_trait_def_id};
use crate::functions::result::result_err_ty;

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

fn is_overly_generic_error_type(cx: &LateContext<'_>, ty: Ty<'_>) -> bool {
    if is_type_lang_item(cx, ty.clone(), LangItem::String) {
        return true;
    }
    if is_type_lang_item(cx, ty.clone(), LangItem::OwnedBox) {
        let inner = ty.boxed_ty();
        if let TyKind::Dynamic(predicates, _, _) = inner.kind() {
            if predicates.iter().any(|predicate| if let ExistentialPredicate::Trait(trait_) = predicate.skip_binder() {
                trait_.def_id == get_trait_def_id(cx, &["core", "error", "Error"]).unwrap()
            } else { false }) {
                return true
            }
        }
    }
    false
}

impl<'tcx> LateLintPass<'tcx> for LibraryCratesErrorEnum {
    fn check_fn(&mut self,
                cx: &LateContext<'tcx>,
                _: rustc_hir::intravisit::FnKind<'tcx>,
                fn_: &'tcx FnDecl<'tcx>,
                _: &'tcx Body<'tcx>,
                span: Span,
                local_def_id: LocalDefId) {
        // span.ctxt().edition().
        //We are looking for functions that return anyhow::Result or
        // Result<_, Box<dyn Error>> or Result<_, String>
        if let Some((hir_ty, err_ty)) = result_err_ty(cx, fn_, local_def_id, span) {
            if is_overly_generic_error_type(cx, err_ty) {
                span_lint_and_note(cx, LIBRARY_CRATES_ERROR_ENUM, hir_ty.span, "This is an overly generic error type", None, "Try using an error enum");
            }
        }
    }
}