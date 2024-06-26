#' Load a blueprint from a script file
#'
#' @param plan A drake plan
#' @param file A path to a script file
#' @param directory A path to a directory with script files that are blueprints.
#'                  Defaults to the "blueprints" directory at the root of the
#'                  current R project.
#' @param recurse Recursively loads blueprints from a directory if `TRUE`
#'
#' @details # Empty blueprint folder
#' By default, blueprintr ignore empty blueprint folders. However, it may be beneficial
#' to warn users if folder is empty, particularly during project setup. This helps
#' identify any potential misconfiguration of drake plan attachment. To enable these warnings,
#' set `option(blueprintr.warn_empty_blueprints_dirs = TRUE)`.
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
load_blueprints <- function(plan,
                            directory = here::here("blueprints"),
                            recurse = FALSE) {
  bp_assert(inherits(plan, "drake_plan"))

  dirs <- load_dirs_recurse(directory, recurse)
  bp_list <- fetch_blueprints_from_dir(dirs)

  if (is.null(bp_list)) {
    return(plan)
  }

  attach_blueprints(plan, !!!bp_list)
}

fetch_blueprints_from_dir <- function(dirs) {
  bp_list <- unlist(lapply(dirs, fetch_blueprint_files))
  lapply(bp_list, import_blueprint_file)
}

load_dirs_recurse <- function(dir, recurse) {
  subdirs <- list.dirs(path = dir)

  if (length(subdirs) > 0 && isTRUE(recurse)) {
    dirs <- c(dir, subdirs)
  } else {
    dirs <- dir
  }

  dirs
}

fetch_blueprint_files <- function(directory) {
  if (!dir.exists(directory)) {
    bp_err("Blueprint directory '{directory}' does not exist")
  }

  bp_scripts <- list.files(path = directory, full.names = TRUE, pattern = "\\.[Rr]$")

  if (length(bp_scripts) == 0L) {
    if (warn_empty_blueprints_dir()) {
      bp_warn("No blueprint scripts found in '{directory}'")
    }
    return(NULL)
  }

  bp_scripts
}

warn_empty_blueprints_dir <- function() {
  getOption(empty_blueprints_dir_option(), default = FALSE)
}

empty_blueprints_dir_option <- function() {
  "blueprintr.warn_empty_blueprints_dirs"
}

import_blueprint_file <- function(bp_file, env = parent.frame()) {
  env$cur_blueprint_script <- bp_file

  exprs <- rlang::parse_exprs(file(bp_file, encoding = "UTF-8"))
  vals <- lapply(exprs, base::eval, envir = env)

  if (length(vals) < 1) {
    bp_err("Blueprint script '{bp_file}' has no content.")
  }

  script_val <- vals[[length(vals)]]

  if (!inherits(script_val, "blueprint")) {
    bp_err("Blueprint script '{bp_file}' does not evaluate to a blueprint")
  }

  script_val
}
