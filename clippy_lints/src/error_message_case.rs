use std::collections::HashMap;
use clippy_config::types::ErrorMessageCaseBehavior;
use clippy_utils::{get_parent_as_impl, MaybePath};
use rustc_data_structures::fx::FxHashMap;
use rustc_hir::*;
use rustc_hir::intravisit::FnKind;
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::mir::traversal::Postorder;
use rustc_session::{declare_lint_pass, impl_lint_pass};
use rustc_span::def_id::LocalDefId;
use rustc_span::{Span, sym, Symbol};
use rustc_target::asm::X86InlineAsmReg::bl;

declare_clippy_lint! {
    /// ### What it does
    ///
    /// ### Why is this bad?
    ///
    /// ### Example
    /// ```no_run
    /// // example code where clippy issues a warning
    /// ```
    /// Use instead:
    /// ```no_run
    /// // example code which does not raise clippy warning
    /// ```
    #[clippy::version = "1.77.0"]
    pub ERROR_MESSAGE_CASE,
    nursery,
    "default lint description"
}

impl_lint_pass!(ErrorMessageCase => [ERROR_MESSAGE_CASE]);


#[derive(Copy, Clone)]
pub enum PrintBehavior {
    StartsWithUppercase,
    StartsWithLowercase,
    StartsWithVariable(HirId),
    NoPrint,
    Unknown,
}

impl PrintBehavior {
    pub fn combine(&self, other: PrintBehavior) -> PrintBehavior {
        match self {
            PrintBehavior::StartsWithUppercase |
            PrintBehavior::StartsWithLowercase |
            PrintBehavior::StartsWithVariable(_) => {
                *self
            }
            PrintBehavior::NoPrint => {
                other
            }
            PrintBehavior::Unknown => {
                PrintBehavior::Unknown
            }
        }
    }
}

#[derive(Copy, Clone)]
pub struct ExpressionId(HirId);

#[derive(Copy, Clone)]
pub struct ErrorMessageCase {
    error_message_case_behavior: ErrorMessageCaseBehavior,
    inner: HashMap<ExpressionId, PrintBehavior>,
    // Whether we are inside Display or Debug trait impl - None for neither
    format_trait_impl: Option<FormatTraitNames>,
}

impl ErrorMessageCase {
    #[must_use]
    pub fn new(error_message_case_behavior: ErrorMessageCaseBehavior) -> Self {
        Self {
            error_message_case_behavior,
            inner: HashMap::new(),
            format_trait_impl: None,
        }
    }
}

#[derive(Clone, Copy)]
struct FormatTraitNames {
    /// e.g. `sym::Display`
    name: Symbol,
    /// `f` in `fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {}`
    formatter_name: Option<Symbol>,
}

fn expr_print_behavior_many(arrays: &'_ [impl AsRef<Expr<'_>>]) -> PrintBehavior {
    arrays.iter().fold(PrintBehavior::NoPrint, |acc, expr| {
        acc.combine(expr_print_behavior(expr))
    })
}

fn stmt_print_behvaior_many(arrays: &'_ [impl AsRef<Stmt<'_>>]) -> PrintBehavior {
    arrays.iter().fold(PrintBehavior::NoPrint, |acc, stmt| {
        acc.combine(stmt_print_behavior(stmt))
    })
}

fn stmt_print_behavior(stmt: &'_ Stmt<'_>) -> PrintBehavior {
    match stmt.kind{
        StmtKind::Local(local) => {
            if let Some(expr) = local.init {
                let else_block  = local.els.map(|else_block|{
                    stmt_print_behvaior_many(else_block.stmts)
                }).unwrap_or(PrintBehavior::NoPrint);
                expr_print_behavior(expr).combine(else_block)
            } else {
                PrintBehavior::NoPrint
            }
        }
        StmtKind::Item(_) => {
            PrintBehavior::NoPrint
        }
        StmtKind::Expr(expr) |
        StmtKind::Semi(expr) => {
            expr_print_behavior(&expr)
        }
    }
}

fn block_print_behavior(block: &'_ Block<'_>) -> PrintBehavior {
    if let Some(expr) = block.expr {
        stmt_print_behvaior_many(block.stmts).combine(expr_print_behavior(&expr))
    } else {
        stmt_print_behvaior_many(block.stmts)
    }

}

fn expr_print_behavior(expr: &'_ Expr<'_>) -> PrintBehavior {
    match expr.kind {
        ExprKind::Err(_) |
        ExprKind::OffsetOf(_, _) |
        ExprKind::InlineAsm(_) |
        ExprKind::Continue(_) |
        ExprKind::Path(_) |
        ExprKind::Closure(_) |
        ExprKind::ConstBlock(_) |
        ExprKind::Lit(_) => {
            PrintBehavior::NoPrint
        }
        ExprKind::Tup(elems) |
        ExprKind::Array(elems) => {
            expr_print_behavior_many(elems)
        }
        ExprKind::Index(left, right, _) |
        ExprKind::AssignOp(_, left, right) |
        ExprKind::Assign(left, right, _) |
        ExprKind::Binary(_, left, right) => {
            expr_print_behavior(left).combine(expr_print_behavior(right))
        }
        ExprKind::Yield(inner, _) |
        ExprKind::Repeat(inner, _) |
        ExprKind::AddrOf(_, _, inner) |
        ExprKind::Become(inner) |
        ExprKind::Field(inner, _) |
        ExprKind::Match(inner, _, _) |
        ExprKind::Unary(_, inner) |
        ExprKind::Cast(inner, _) |
        ExprKind::Type(inner, _) |
        ExprKind::DropTemps(inner) => {
            expr_print_behavior(inner)
        }
        ExprKind::Let(let_) => {
            expr_print_behavior(&let_.init)
        }
        ExprKind::If(condition, if_case, else_case) => {
            if let Some(else_case) = else_case {
                expr_print_behavior_many(&[condition,if_case, else_case])
            } else {
                expr_print_behavior_many(&[condition,if_case])
            }
        }
        ExprKind::Loop(block, _, _, _) |
        ExprKind::Block(block, _) => {
            block_print_behavior(block)
        }
        ExprKind::Break(_, inner_option) |
        ExprKind::Ret(inner_option) => {
            inner_option.map_or(PrintBehavior::NoPrint, expr_print_behavior)
        }
        ExprKind::Struct(_, fields, base) => {
            fields.iter().map(|field|field.expr).chain(base).fold(PrintBehavior::NoPrint, |acc, expr| {
                acc.combine(expr_print_behavior(expr))
            })
        }
        ExprKind::Call(function_expr, args) => {
            let function_print_behavior = expr_print_behavior(function_expr);
            let args_print_behavior = expr_print_behavior_many(args);
            let qpath = function_expr.qpath_opt();
            if let ExprKind::Path(QPath::Resolved(_, path)) = &function_expr.kind
                && let Some(def_id) = path.res.opt_def_id() {
                todo!()
            }

        }
        ExprKind::MethodCall(_, _, _, _) => {}
    }
}

impl LateLintPass<'_> for ErrorMessageCase {
    fn check_impl_item(&mut self, cx: &LateContext<'_>, impl_item: &ImplItem<'_>) {
        self.format_trait_impl = is_format_trait_impl(cx, impl_item);
    }

    fn check_impl_item_post(&mut self, cx: &LateContext<'_>, impl_item: &ImplItem<'_>) {
        // Assume no nested Impl of Debug and Display within eachother
        if is_format_trait_impl(cx, impl_item).is_some() {
            self.format_trait_impl = None;
        }
    }
}

//todo common code with format_impl
//todo need to check that trait is also error
fn is_format_trait_impl(cx: &LateContext<'_>, impl_item: &ImplItem<'_>) -> Option<FormatTraitNames> {
    if impl_item.ident.name == sym::fmt
        && let ImplItemKind::Fn(_, body_id) = impl_item.kind
        && let Some(Impl {
                        of_trait: Some(trait_ref),
                        ..
                    }) = get_parent_as_impl(cx.tcx, impl_item.hir_id())
        && let Some(did) = trait_ref.trait_def_id()
        && let Some(name) = cx.tcx.get_diagnostic_name(did)
        && matches!(name, sym::Display)
    {
        let body = cx.tcx.hir().body(body_id);
        let formatter_name = body
            .params
            .get(1)
            .and_then(|param| param.pat.simple_ident())
            .map(|ident| ident.name);

        Some(FormatTraitNames { name, formatter_name })
    } else {
        None
    }
}

