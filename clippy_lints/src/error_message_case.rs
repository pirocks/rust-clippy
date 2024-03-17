use std::collections::HashMap;
use clippy_config::types::ErrorMessageCaseBehavior;
use clippy_utils::{get_parent_as_impl, MaybePath};
use rustc_hir::*;
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::impl_lint_pass;
use rustc_span::{MacroKind, sym, Symbol};
use clippy_utils::macros::macro_backtrace;

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
        acc.combine(expr_print_behavior(cx,format_trait_names, expr))
    })
}

fn stmt_print_behvaior_many<'a>(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, arrays: &'_ [Stmt<'a>]) -> PrintBehavior {
    arrays.iter().fold(PrintBehavior::NoPrint, |acc, stmt| {
        acc.combine(stmt_print_behavior(cx,format_trait_names, stmt))
    })
}

fn stmt_print_behavior(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, stmt: &'_ Stmt<'_>) -> PrintBehavior {
    match stmt.kind{
        StmtKind::Local(local) => {
            if let Some(expr) = local.init {
                let else_block  = local.els.map(|else_block|{
                    stmt_print_behvaior_many(cx, format_trait_names,else_block.stmts)
                }).unwrap_or(PrintBehavior::NoPrint);
                expr_print_behavior(cx, format_trait_names,expr).combine(else_block)
            } else {
                PrintBehavior::NoPrint
            }
        }
        StmtKind::Item(_) => {
            PrintBehavior::NoPrint
        }
        StmtKind::Expr(expr) |
        StmtKind::Semi(expr) => {
            expr_print_behavior(cx,format_trait_names, &expr)
        }
    }
}

fn block_print_behavior(cx: &LateContext<'_>,format_trait_names: &FormatTraitNames, block: &'_ Block<'_>) -> PrintBehavior {
    if let Some(expr) = block.expr {
        stmt_print_behvaior_many(cx, format_trait_names,&block.stmts).combine(expr_print_behavior(cx,format_trait_names, &expr))
    } else {
        stmt_print_behvaior_many(cx,format_trait_names, &block.stmts)
    }

}

fn expr_print_behavior(cx: &LateContext<'_>, format_trait_names: &FormatTraitNames, expr: &'_ Expr<'_>) -> PrintBehavior {
    for macro_call in macro_backtrace(expr.span){
        let macro_name = cx.tcx.item_name(macro_call.def_id);
        let name_matches = macro_name == sym::format_args_macro;
        let is_bang_macro = matches!(macro_call.kind, MacroKind::Bang);
        if name_matches && is_bang_macro {
            return PrintBehavior::NoPrint;
        }
    }
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
            expr_print_behavior_many(cx, format_trait_names,elems)
        }
        ExprKind::Index(left, right, _) |
        ExprKind::AssignOp(_, left, right) |
        ExprKind::Assign(left, right, _) |
        ExprKind::Binary(_, left, right) => {
            expr_print_behavior(cx,format_trait_names, left).combine(expr_print_behavior(cx,format_trait_names, right))
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
            expr_print_behavior(cx,format_trait_names, inner)
        }
        ExprKind::Let(let_) => {
            expr_print_behavior(cx,format_trait_names, &let_.init)
        }
        ExprKind::If(condition, if_case, else_case) => {
            if let Some(else_case) = else_case {
                expr_print_behavior_many(cx, format_trait_names,[condition,if_case, else_case])
            } else {
                expr_print_behavior_many(cx, format_trait_names,[condition,if_case])
            }
        }
        ExprKind::Loop(block, _, _, _) |
        ExprKind::Block(block, _) => {
            block_print_behavior(cx, format_trait_names,block)
        }
        ExprKind::Break(_, inner_option) |
        ExprKind::Ret(inner_option) => {
            inner_option
                .map(|expr|expr_print_behavior(cx,format_trait_names, expr))
                .unwrap_or(PrintBehavior::NoPrint)
        }
        ExprKind::Struct(_, fields, base) => {
            fields.iter().map(|field|field.expr).chain(base).fold(PrintBehavior::NoPrint, |acc, expr| {
                acc.combine(expr_print_behavior(cx, format_trait_names,expr))
            })
        }
        ExprKind::Call(function_expr, args) => {
            let function_print_behavior = expr_print_behavior(cx, format_trait_names, function_expr);
            let args_print_behavior = expr_print_behavior_many(cx,format_trait_names, args);
            let qpath = function_expr.qpath_opt();
            dbg!(qpath);
            match &function_expr.kind{
                ExprKind::Path(QPath::Resolved(_, path)) => {
                    if let Some(def_id) = path.res.opt_def_id()
                        && let def_path = cx.get_def_path(def_id)
                    {
                        let def_path = def_path.iter().map(Symbol::as_str).collect::<Vec<_>>();
                        if dbg!(def_path.as_slice()) == &["core", "fmt", "Write", "write"] {
                            dbg!(args[0]);
                            return todo!();
                        }
                    }
                }
                ExprKind::Path(QPath::TypeRelative(ty, segment)) => {
                    if let Some(def_id) = segment.res.opt_def_id() &&
                        let def_path = cx.get_def_path(def_id) {

                        let def_path = def_path.iter().map(Symbol::as_str).collect::<Vec<_>>();
                        if dbg!(def_path.as_slice()) == &["core", "fmt", "Arguments", "new_const"] {
                            dbg!(args[0]);
                            return todo!();
                        }
                    }
                }
                ExprKind::Path(QPath::LangItem(lang_item,_)) => {
                    dbg!(lang_item);
                }
                _ => panic!("function expr kind sohuld be one of the above")
            }
            todo!()
        }
        ExprKind::MethodCall(method_path, arg, args, span) => {
            //todo do something with span
            let arg_print_behavior = expr_print_behavior(cx, format_trait_names,arg);
            let args_print_behavior = expr_print_behavior_many(cx,format_trait_names, args);
            if let PrintBehavior::NoPrint = arg_print_behavior.combine(args_print_behavior){
                if let Some(def_id) = method_path.res.opt_def_id(){
                    let def_path = cx.get_def_path(def_id);
                    let def_path = def_path.iter().map(Symbol::as_str).collect::<Vec<_>>();
                    if dbg!(def_path.as_slice()) == &["core", "fmt", "Write", "write"] {
                        dbg!(args[0]);
                        return PrintBehavior::StartsWithLowercase;
                    }
                }
            }
            todo!()
        }
    }
}

impl LateLintPass<'_> for ErrorMessageCase {
    fn check_impl_item(&mut self, cx: &LateContext<'_>, impl_item: &ImplItem<'_>) {
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

            let format_trait_names = FormatTraitNames { name, formatter_name };
            let behavior = expr_print_behavior(cx, &format_trait_names, &body.value);
            match behavior {
                PrintBehavior::StartsWithUppercase => {
                    if self.error_message_case_behavior == ErrorMessageCaseBehavior::Lower {
                        todo!()
                    }
                }
                PrintBehavior::StartsWithLowercase => {
                    if self.error_message_case_behavior == ErrorMessageCaseBehavior::Upper {
                        todo!()
                    }
                }
                PrintBehavior::StartsWithVariable(_) => {
                    //todo perhaps we can resolve this later.
                }
                PrintBehavior::NoPrint => {}
                PrintBehavior::Unknown => {

                }
            }
        }
    }
}
