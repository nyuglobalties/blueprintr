find_ast_if <- function(ast, .p, recurse = TRUE) {
  if (!is_ast(ast)) {
    if (isTRUE(.p(ast))) {
      return(extract_ast(ast))
    } else {
      return(list())
    }
  } else {
    out <- list()

    if (isTRUE(.p(ast))) {
      out <- list(ast)
    } else {
      if (isTRUE(recurse)) {
        out <- lapply(ast$args, find_ast_if, .p)
      }
    }

    out
  }
}
