#' Get the parent datasets' names for a blueprint
#'
#' @param blueprint A blueprint object
#' @return A character vector of the names of the parent datasets
#' @export
blueprint_deps <- function(blueprint) {
  UseMethod("blueprint_deps")
}

#' @export
blueprint_deps.default <- function(blueprint) {
  bp_err("Not defined")
}

#' @export
blueprint_deps.blueprint <- function(blueprint) {
  bp_assert(inherits(blueprint, "blueprint"))

  command_ast <- extract_ast(blueprint$command)

  target_calls <- find_ast_if(command_ast, target_call_check)
  target_names <- unlist(target_calls)

  if (is.null(target_names)) {
    return(character())
  }

  unname(target_names[target_names != ".TARGET"])
}
