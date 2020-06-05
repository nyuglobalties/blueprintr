# For now, just an alias to as.name, but will allow for more
# structured drake target names with configuration
.BLUEPRINT <- function(bp_name, ...) {
  as.name(bp_name)
}

is_blueprint_ast <- function(ast) {
  if (!is_ast(ast)) {
    return(FALSE)
  }

  identical(ast$head, ".BLUEPRINT")
}

eval_blueprint_ast <- function(ast, env = parent.frame()) {
  collapsed <- collapse_ast(ast)

  eval_tidy(collapsed, env = env)
}

blueprint_deps <- function(blueprint) {
  bp_assert(inherits(blueprint, "blueprint"))

  command_ast <- extract_ast(blueprint$command)

  blueprint_calls <- find_ast_if(command_ast, blueprint_call_check)
  blueprint_names <- unlist(blueprint_calls)

  if (is.null(blueprint_names)) {
    return(character())
  }
  
  unname(blueprint_names[blueprint_names != ".BLUEPRINT"])
}

blueprint_call_check <- function(ast) {
  if (is_ast(ast)) {
    identical(ast$head, ".BLUEPRINT")
  } else {
    FALSE
  } 
}
