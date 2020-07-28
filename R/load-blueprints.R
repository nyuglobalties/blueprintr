#' Load a blueprint from a script file
#'
#' @param plan A drake plan
#' @param file A path to a script file
#' @param directory A path to a directory with script files that are blueprints.
#'                  Defaults to the "blueprints" directory at the root of the
#'                  current R project.
#'
#' @return A drake_plan with attached blueprints
#' @export
load_blueprint <- function(plan, file) {
  bp_assert(inherits(plan, "drake_plan"))

  if (!file.exists(file)) {
    bp_err("Expected blueprint file '{file}' does not exist")
  }

  attach_blueprint(plan, import_blueprint_file(file))
}

#' @rdname load_blueprint
#' @export
load_blueprints <- function(plan, directory = here::here("blueprints")) {
  bp_assert(inherits(plan, "drake_plan"))

  if (!dir.exists(directory)) {
    bp_err("Blueprint directory '{directory}' does not exist")
  }

  bp_scripts <- grep("\\.[Rr]$", list.files(directory), value = TRUE)
  bp_scripts <- file.path(directory, bp_scripts)

  if (length(bp_scripts) == 0L) {
    bp_warn("No blueprint scripts found in '{directory}'")
    return(plan)
  }

  bp_list <- lapply(bp_scripts, import_blueprint_file)
  attach_blueprints(plan, !!!bp_list)
}

import_blueprint_file <- function(bp_file, env = parent.frame()) {
  exprs <- rlang::parse_exprs(file(bp_file))
  vals <- lapply(exprs, eval_tidy, env = env)

  if (length(vals) < 1) {
    bp_err("Blueprint script '{bp_file}' has no content.")
  }

  script_val <- vals[[length(vals)]]

  if (!inherits(script_val, "blueprint")) {
    bp_err("Blueprint script '{bp_file}' does not evaluate to a blueprint")
  }

  script_val
}
