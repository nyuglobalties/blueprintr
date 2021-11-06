modify_ast_if <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  UseMethod("modify_ast_if", .ast)
}

#' @export
modify_ast_if.default <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  if (isTRUE(.p(.ast))) {
    return(.f(.ast, ...))
  } else {
    return(.ast)
  }
}

#' @export
modify_ast_if.ast <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  # Do a deep search. Presumably, .p(.ast, ...) will not be true for
  # child elements, if the parent was intended to be changed. This
  # design will make the logic more straightforward.

  if (isTRUE(.recurse)) {
    .ast$args <- lapply(.ast$args, modify_ast_if, .p, .f, ..., .recurse = .recurse)
  }

  if (isTRUE(.p(.ast))) {
    .ast <- .f(.ast, ...)
  }

  .ast
}

#' @export
modify_ast_if.function_ast <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  # Do a deep search. Presumably, .p(.ast, ...) will not be true for
  # child elements, if the parent was intended to be changed. This
  # design will make the logic more straightforward.

  if (isTRUE(.recurse)) {
    .ast$args <- modify_ast_if(.ast$args, .p, .f, ..., .recurse = .recurse)
    .ast$fargs[mutable_fargs(.ast)] <-
      lapply(.ast$fargs[mutable_fargs(.ast)], modify_ast_if, .p, .f, ..., .recurse = .recurse)
  }

  if (isTRUE(.p(.ast))) {
    .ast <- .f(.ast, ...)
  }

  .ast
}

#' @export
modify_ast_if.qualified_ast <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  # Do a deep search. Presumably, .p(.ast, ...) will not be true for
  # child elements, if the parent was intended to be changed. This
  # design will make the logic more straightforward.

  if (isTRUE(.recurse)) {
    .ast$args <- lapply(.ast$args, modify_ast_if, .p, .f, ..., .recurse = .recurse)
    .ast$qual_head <- modify_ast_if(.ast$qual_head, .p, .f, ..., .recurse = .recurse)
  }

  if (isTRUE(.p(.ast))) {
    .ast <- .f(.ast, ...)
  }

  .ast
}

#' @export
modify_ast_if.formula_ast <- function(.ast, .p, .f, ..., .recurse = TRUE) {
  # Do a deep search. Presumably, .p(.ast, ...) will not be true for
  # child elements, if the parent was intended to be changed. This
  # design will make the logic more straightforward.

  if (isTRUE(.recurse)) {
    .ast$args <- modify_ast_if(.ast$args, .p, .f, ..., .recurse = .recurse)
  }

  if (isTRUE(.p(.ast))) {
    .ast <- .f(.ast, ...)
  }

  .ast
}
