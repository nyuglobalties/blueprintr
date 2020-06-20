.TARGET <- function(bp_name) {
  as.name(blueprint_final_name(bp_name))
}

.BLUEPRINT <- function(bp_name) {
  as.name(blueprint_reference_name(bp_name))
}

.META <- function(bp_name) {
  as.name(metadata_target_name(bp_name))
}

is_macro_ast <- function(ast, .macro = ".TARGET") {
  if (!is_ast(ast)) {
    return(FALSE)
  }

  ast$head %in% .macro
}

is_target_ast <- function(ast) {
  is_macro_ast(ast)
}

is_blueprint_ast <- function(ast) {
  is_macro_ast(ast, ".BLUEPRINT")
}

is_meta_ast <- function(ast) {
  is_macro_ast(ast, ".META")
}

eval_ast <- function(ast, env = parent.frame()) {
  collapsed <- collapse_ast(ast)

  eval_tidy(collapsed, env = env)
}

blueprint_deps <- function(blueprint) {
  bp_assert(inherits(blueprint, "blueprint"))

  command_ast <- extract_ast(blueprint$command)

  target_calls <- find_ast_if(command_ast, target_call_check)
  target_names <- unlist(target_calls)

  if (is.null(target_names)) {
    return(character())
  }
  
  unname(target_names[target_names != ".TARGET"])
}

target_call_check <- function(ast) {
  if (is_ast(ast)) {
    identical(ast$head, ".TARGET")
  } else {
    FALSE
  } 
}
