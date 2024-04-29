collapse_ast <- function(ex) {
  UseMethod("collapse_ast", ex)
}

#' @export
collapse_ast.default <- function(ex) {
  ex
}

#' @export
collapse_ast.ast <- function(ex) {
  collapsed_args <- lapply(ex$args, collapse_ast)

  rlang::call2(ex$head, !!!collapsed_args)
}

#' @export
collapse_ast.qualified_ast <- function(ex) {
  collapsed_args <- lapply(ex$args, collapse_ast)

  if (!is_namespaced_ast(ex)) {
    collapsed_head <- collapse_ast(ex$qual_head)

    ncar <- rlang::call2(ex$qual_sym, collapsed_head, ex$head)
    rlang::call2(ncar, !!!collapsed_args)
  } else {
    if (identical(ex$qual_sym, quote(`::`))) {
      rlang::call2(ex$head, !!!collapsed_args, .ns = ex$ns)
    } else {
      rlang::call2(ex$qual_sym, ex$qual_head, rlang::call2(ex$head, !!!collapsed_args))
    }
  }
}

#' @export
collapse_ast.function_ast <- function(ex) {
  collapsed_body <- collapse_ast(ex$args)
  ex$fargs[mutable_fargs(ex)] <- lapply(ex$fargs[mutable_fargs(ex)], collapse_ast)
  rlang::call2("function", as.pairlist(ex$fargs), collapsed_body)
}

#' @export
collapse_ast.formula_ast <- function(ex) {
  if (length(ex$args) > 1) {
    lhs <- collapse_ast(ex$args[[1]])
    rhs <- collapse_ast(ex$args[[2]])
    bquote(.(lhs) ~ .(rhs))
  } else {
    form <- collapse_ast(ex$args[[1]])
    bquote(~ .(form))
  }
}
