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
#' @return A `list()` of `tar_target` objects
#'
#' @export
tar_blueprint <- function(...) {
  bp <- blueprint(...)
  steps <- assembly_steps(targets_assembler(), bp)

  lapply(steps, function(step) step$payload)
}

#' @rdname tar_blueprint
#' @export
tar_blueprints <- function(directory = here::here("blueprints")) {
  bp_list <- fetch_blueprint_files(directory)

  if (is.null(bp_list)) {
    return(list())
  }

  raw_targets <- lapply(bp_list, tar_blueprint_raw)
  flatten(raw_targets)
}

tar_blueprint_raw <- function(bp) {
  steps <- assembly_steps(targets_assembler(), bp)

  lapply(steps, function(step) step$payload)
}
