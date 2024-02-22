use std::collections::HashMap;
use clippy_config::types::ErrorMessageCaseBehavior;
use clippy_utils::{get_parent_as_impl, MaybePath};
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::impl_lint_pass;
use rustc_span::{sym, Symbol};declare_clippy_lint! {
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

#[derive(Clone)]
pub struct ErrorMessageCase {
    pub(crate) error_message_case_behavior: ErrorMessageCaseBehavior,
    pub(crate) inner: HashMap<ExpressionId, PrintBehavior>,
    // Whether we are inside Display or Debug trait impl - None for neither
    pub(crate) format_trait_impl: Option<FormatTraitNames>,
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

fn expr_print_behavior_many<'a>(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, arrays: impl IntoIterator<Item=&'a Expr<'a>>) -> PrintBehavior {
    arrays.into_iter().fold(PrintBehavior::NoPrint, |acc, expr| {
        acc.combine(expr_print_behavior(cx, expr))
    })
}

fn stmt_print_behvaior_many<'a>(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, arrays: &'_ [Stmt<'a>]) -> PrintBehavior {
    arrays.iter().fold(PrintBehavior::NoPrint, |acc, stmt| {
        acc.combine(stmt_print_behavior(cx, stmt))
    })
}

fn stmt_print_behavior(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, stmt: &'_ Stmt<'_>) -> PrintBehavior {
    match stmt.kind{
        StmtKind::Local(local) => {
            if let Some(expr) = local.init {
                let else_block  = local.els.map(|else_block|{
                    stmt_print_behvaior_many(cx, else_block.stmts)
                }).unwrap_or(PrintBehavior::NoPrint);
                expr_print_behavior(cx, expr).combine(else_block)
            } else {
                PrintBehavior::NoPrint
            }
        }
        StmtKind::Item(_) => {
            PrintBehavior::NoPrint
        }
        StmtKind::Expr(expr) |
        StmtKind::Semi(expr) => {
            expr_print_behavior(cx, &expr)
        }
    }
}

fn block_print_behavior(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, block: &'_ Block<'_>) -> PrintBehavior {
    if let Some(expr) = block.expr {
        stmt_print_behvaior_many(cx, &block.stmts).combine(expr_print_behavior(cx, &expr))
    } else {
        stmt_print_behvaior_many(cx, &block.stmts)
    }

}

fn expr_print_behavior(cx: &LateContext<'_>, format_trait_names: &FormatTraitNames, expr: &'_ Expr<'_>) -> PrintBehavior {
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
            expr_print_behavior_many(cx, elems)
        }
        ExprKind::Index(left, right, _) |
        ExprKind::AssignOp(_, left, right) |
        ExprKind::Assign(left, right, _) |
        ExprKind::Binary(_, left, right) => {
            expr_print_behavior(cx, left).combine(expr_print_behavior(cx, right))
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
            expr_print_behavior(cx, inner)
        }
        ExprKind::Let(let_) => {
            expr_print_behavior(cx, &let_.init)
        }
        ExprKind::If(condition, if_case, else_case) => {
            if let Some(else_case) = else_case {
                expr_print_behavior_many(cx, [condition,if_case, else_case])
            } else {
                expr_print_behavior_many(cx, [condition,if_case])
            }
        }
        ExprKind::Loop(block, _, _, _) |
        ExprKind::Block(block, _) => {
            block_print_behavior(cx, block)
        }
        ExprKind::Break(_, inner_option) |
        ExprKind::Ret(inner_option) => {
            inner_option
                .map(|expr|expr_print_behavior(cx, expr))
                .unwrap_or(PrintBehavior::NoPrint)
        }
        ExprKind::Struct(_, fields, base) => {
            fields.iter().map(|field|field.expr).chain(base).fold(PrintBehavior::NoPrint, |acc, expr| {
                acc.combine(expr_print_behavior(cx, expr))
            })
        }
        ExprKind::Call(function_expr, args) => {
            let function_print_behavior = expr_print_behavior(cx, function_expr);
            let args_print_behavior = expr_print_behavior_many(cx, args);
            let qpath = function_expr.qpath_opt();
            if let ExprKind::Path(QPath::Resolved(_, path)) = &function_expr.kind
                && let Some(def_id) = path.res.opt_def_id()
                && let def_path = cx.get_def_path(def_id)
                && let def_path = def_path.iter().map(Symbol::as_str).collect::<Vec<_>>()
            {
                if def_path == &[sym::core, sym::option, sym::Write, sym::write] {
                    return args_print_behavior;

                }
                format_trait_names.formatter_name
                todo!()
            }

        }
        ExprKind::MethodCall(method_path, arg, args, span) => {
            //todo do something with span
            let arg_print_behavior = expr_print_behavior(cx, format_trait_names,arg);
            let args_print_behavior = expr_print_behavior_many(cx,format_trait_names, args);
            if let PrintBehavior::NoPrint = arg_print_behavior.combine(args_print_behavior){
                if let Some(def_id) = method_path.res.opt_def_id(){
                    let def_path = cx.get_def_path(def_id);
                    let def_path = def_path.iter().map(Symbol::as_str).collect::<Vec<_>>();
                    if def_path == &["core", "fmt", "Write", "write"] {
                        args[0]
                        return PrintBehavior::StartsWithLowercase;
                    }
                }
            }
        }
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

