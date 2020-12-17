#' @export
tar_blueprint <- function(...) {
  asm <- targets_assembler()
  bp <- blueprint(...)
  steps <- assembly_steps(asm, bp)

  lapply(steps, function(step) step$payload)
}

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
  asm <- targets_assembler()
  steps <- assembly_steps(asm, bp)

  lapply(steps, function(step) step$payload)
}
