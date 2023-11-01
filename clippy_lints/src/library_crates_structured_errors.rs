use clippy_utils::diagnostics::span_lint_and_note;
use clippy_utils::match_def_path;
use clippy_utils::ty::{is_type_lang_item, result_err_ty};
use rustc_hir::intravisit::FnKind;
use rustc_hir::{Body, FnDecl, LangItem};
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty;
use rustc_middle::ty::{ExistentialPredicate, Ty};
use rustc_session::config::CrateType;
use rustc_session::impl_lint_pass;
use rustc_span::def_id::LocalDefId;
use rustc_span::{sym, Span};

declare_clippy_lint! {
    /// ### What it does
    /// Finds usages of unstructured error types in library crates.
    ///
    /// ### Why is this bad?
    /// Libraries should use structured error types to allow users to
    /// match on different error cases.
    ///
    /// ### Known problems
    /// This only detects certain kinds of unstructured error types,
    /// not all of them.
    ///
    /// ### Example
    /// Before:
    /// ```no_run
    /// use std::error::Error;
    /// fn foo() -> Result<(), Box<dyn Error>> {
    ///    todo!()
    /// }
    /// ```
    ///
    /// After:
    /// ```no_run
    /// struct MyError;
    /// fn foo() -> Result<(), MyError> {
    ///   todo!()
    /// }
    /// ```
    #[clippy::version = "1.77.0"]
    pub LIBRARY_CRATES_STRUCTURED_ERRORS,
    restriction,
    "library crates that use unstructured error types"
}

#[derive(Default)]
pub struct LibraryCratesStructuredErrors {
    is_library_crate: Option<bool>,
}

impl_lint_pass!(LibraryCratesStructuredErrors => [LIBRARY_CRATES_STRUCTURED_ERRORS]);

fn is_overly_generic_error_type(cx: &LateContext<'_>, ty: Ty<'_>) -> bool {
    if is_type_lang_item(cx, ty, LangItem::String) {
        return true;
    }
    if is_type_lang_item(cx, ty, LangItem::OwnedBox) {
        let inner = ty.boxed_ty();
        if let ty::Dynamic(predicates, _, _) = inner.kind() {
            if predicates.iter().any(|predicate| {
                if let ExistentialPredicate::Trait(trait_) = predicate.skip_binder() {
                    cx.tcx.is_diagnostic_item(sym::Error, trait_.def_id)
                } else {
                    false
                }
            }) {
                return true;
            }
        }
    }
    if match ty.kind() {
        ty::Adt(adt, _) => {
            match_def_path(cx, adt.did(), &["anyhow", "Error"]) || match_def_path(cx, adt.did(), &["eyre", "Report"])
        },
        _ => false,
    } {
        return true;
    }
    false
}

fn is_library_crate(cx: &LateContext<'_>) -> bool {
    cx.tcx.crate_types().iter().any(|t: &CrateType| {
        matches!(
            t,
            CrateType::Cdylib | CrateType::Dylib | CrateType::Rlib | CrateType::ProcMacro | CrateType::Staticlib
        )
    })
}

impl<'tcx> LateLintPass<'tcx> for LibraryCratesStructuredErrors {
    fn check_crate(&mut self, cx: &LateContext<'tcx>) {
        if is_library_crate(cx) {
            self.is_library_crate = Some(true);
        } else {
            self.is_library_crate = Some(false);
        }
    }

    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        fn_kind: FnKind<'tcx>,
        fn_: &'tcx FnDecl<'tcx>,
        _: &'tcx Body<'tcx>,
        span: Span,
        local_def_id: LocalDefId,
    ) {
        if !self
            .is_library_crate
            .expect("Should have been initialized in check_crate")
        {
            return;
        }
        //We are looking for functions that return anyhow::Result or
        // Result<_, Box<dyn Error>> or Result<_, String>
        if let FnKind::Method(_, _) | FnKind::ItemFn(_, _, _) = fn_kind {
            if let Some((hir_ty, err_ty)) = result_err_ty(cx, fn_, local_def_id, span)
                && is_overly_generic_error_type(cx, err_ty)
            {
                span_lint_and_note(
                    cx,
                    LIBRARY_CRATES_STRUCTURED_ERRORS,
                    hir_ty.span,
                    "this is an unstructured error type",
                    None,
                    "try using an error enum",
                );
            }
        }
    }
}
