import_blueprint_file <- function(bp_file, env = parent.frame()) {
  exprs <- parse_exprs(file(bp_file))
  vals <- lapply(exprs, eval_tidy, env = env)

  if (length(vals) < 1) {
    bp_err("Blueprint script '{bp_file}' has no content.")
  }

  script_val <- vals[[length(vals)]]

  if (!check_content_type(script_val)) {
    bp_err("Blueprint script '{bp_file}' does not evaluate to a blueprint or list of blueprints.")
  }

  script_val
}

check_content_type <- function(x) {
  if (inherits(x, "blueprint")) {
    TRUE
  } else {
    if (!is.list(x)) {
      FALSE
    } else {
      all(vlapply(x, function(.x) inherits(.x, "blueprint")))
    }
  }
}
