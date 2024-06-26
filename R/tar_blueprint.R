#' Add a blueprint to a "targets" pipeline
#'
#' Unlike drake, which requires some extra metaprogramming to "attach" blueprint
#' steps to a plan, targets pipelines allow for direct target construction.
#' Blueprints can thus be added directly into a `tar_pipeline()` object using
#' this function. The arguments for `tar_blueprint()` are exactly the same as
#' `blueprint()`. `tar_blueprints()` behaves like `load_blueprints()` but is
#' called, like `tar_blueprint()`, directly in a `tar_pipeline()` object.
#'
#' @param ... Arguments passed to `blueprint()`
#' @param directory A folder containing R scripts that evaluate to `blueprint()`
#'                  objects
#' @param recurse Recursively loads blueprints from a directory if `TRUE`
#' @param bp A blueprint object
#' @return A `list()` of `tar_target` objects

#' @details # Empty blueprint folder
#' By default, blueprintr ignore empty blueprint folders. However, it may be beneficial
#' to warn users if folder is empty, particularly during project setup. This helps
#' identify any potential misconfiguration of targets generation. To enable these warnings,
#' set `option(blueprintr.warn_empty_blueprints_dirs = TRUE)`.
#'
#' @export
tar_blueprint <- function(...) {
  bp <- blueprint(...)
  tar_blueprint_raw(bp)
}

#' @rdname tar_blueprint
#' @export
tar_blueprints <- function(directory = here::here("blueprints"),
                           recurse = FALSE) {
  dirs <- load_dirs_recurse(directory, recurse)
  bp_list <- fetch_blueprints_from_dir(dirs)

  if (is.null(bp_list)) {
    return(list())
  }

  raw_targets <- lapply(bp_list, tar_blueprint_raw)
  flatten(raw_targets)
}

#' @rdname tar_blueprint
#' @export
tar_blueprint_raw <- function(bp) {
  # Suppress loading objects if in interactive macro eval mode
  old_state <- options(blueprintr.attach_state = TRUE)
  on.exit(options(old_state))
  steps <- assembly_steps(targets_assembler(), bp)

  payloads <- lapply(steps, function(step) step$built_payload)

  payloads
}
