#' Translate blueprint commands into executable code
#'
#' Turns code from a blueprint -- of any class -- into an executable command to
#' be run in a drake plan.
#'
#' @param blueprint A blueprint object
#' @export
translate_command <- function(blueprint) {
  UseMethod("translate_command")
}

translate_command.default <- function(blueprint) {
  bp_err("Not defined")
}

#' @export
translate_command.blueprint <- function(blueprint) {
  command <- blueprint$command
  command_ast <- extract_ast(command)
  command_ast <- modify_ast_if(command_ast, is_any_macro_ast, eval_ast)
  collapse_ast(command_ast)
}

#' @export
translate_command.stata_blueprint <- function(blueprint) {
  bp_assert(
    is.character(blueprint$command),
    "STATA commands must be a single string"
  )
}
