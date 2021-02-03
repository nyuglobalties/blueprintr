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

  call2(ex$head, !!!collapsed_args)
}

#' @export
collapse_ast.qualified_ast <- function(ex) {
  collapsed_args <- lapply(ex$args, collapse_ast)

  if (!is_namespaced_ast(ex)) {
    collapsed_head <- collapse_ast(ex$qual_head)

    call2(ex$qual_sym, collapsed_head, call2(ex$head, !!!collapsed_args))
  } else {
    if (identical(ex$qual_sym, quote(`::`))) {
      call2(ex$head, !!!collapsed_args, .ns = ex$ns)
    } else {
      call2(ex$qual_sym, ex$qual_head, call2(ex$head, !!!collapsed_args))
    }
  }
}

#' @export
collapse_ast.function_ast <- function(ex) {
  collapsed_body <- collapse_ast(ex$args)
  ex$fargs[mutable_fargs(ex)] <- lapply(ex$fargs[mutable_fargs(ex)], collapse_ast)
  call2("function", as.pairlist(ex$fargs), collapsed_body)
}
