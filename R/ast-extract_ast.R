extract_ast <- function(ex) {
  UseMethod("extract_ast", ex)
}

#' @export
extract_ast.default <- function(ex) {
  # Base cases
  if (is_leaf(ex)) {
    return(ex)
  }

  extract_ast(ast(ex))
}

#' @export
extract_ast.ast <- function(ex) {
  ex$args <- lapply(ex$args, extract_ast)
  ex
}

#' @export
extract_ast.function_ast <- function(ex) {
  ex$args <- extract_ast(ex$args)
  ex$fargs[mutable_fargs(ex)] <- lapply(ex$fargs[mutable_fargs(ex)], extract_ast)
  ex
}

#' @export
extract_ast.formula_ast <- function(ex) {
  ex$args <- lapply(ex$args, extract_ast)
  ex
}