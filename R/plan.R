#' Create a drake plan from a blueprint
#'
#' Creates a new drake plan from a blueprint
#'
#' @param blueprint A blueprint
#' @return A drake plan with all of the necessary blueprint steps
#' @export
plan_from_blueprint <- function(blueprint) {
  plan <- drake::drake_plan()

  # drake_plan() does not add the class by default
  plan <- structure(
    plan,
    class = c("drake_plan", class(plan))
  )

  attach_blueprint(plan, blueprint)
}

#' @rdname attach_blueprint
#' @export
attach_blueprints <- function(plan, ...) {
  dots <- dots_list(...)

  for (blueprint in dots) {
    plan <- attach_blueprint(plan, blueprint)
  }

  plan
}

#' Attach blueprints to a drake plan
#'
#' Blueprints outline a sequence of checks and cleanup steps
#' that come after a dataset is created. In order for these
#' steps to be executed, the blueprint must be attached to
#' a drake plan so that drake can run these steps properly.
#'
#' @param plan A drake plan
#' @param blueprint A blueprint object
#' @param ... Multiple blueprints
#'
#' @rdname attach_blueprint
#' @export
attach_blueprint <- function(plan, blueprint) {
  stopifnot(inherits(plan, "drake_plan"))
  stopifnot(inherits(blueprint, "blueprint"))

  bp_plan <- blueprint_plan(blueprint)

  dplyr::bind_rows(plan, bp_plan)
}

blueprint_plan <- function(bp) {
  asm <- drake_assembler()

  if (file.exists(metadata_path(bp))) {
    meta_df <- load_metadata(bp)
  } else {
    meta_df <- NULL
  }

  steps <- list(
    bpstep_build_initial(asm, bp),
    bpstep_blueprint_reference(asm, bp),
    bpstep_create_metadata(asm, bp),
    bpstep_load_metadata(asm, bp),
    bpstep_check_data(asm, bp, meta = meta_df),
    bpstep_cleanup(asm, bp)
  )

  if (isTRUE(bp$codebook_export)) {
    steps[[length(steps) + 1]] <- bpstep_export_codebook(asm, bp)
  }

  dplyr::bind_rows(!!!lapply(steps, function(step) step$payload))
}

deparse_lang_cols <- function(plan) {
  for (col in lang_cols(plan)) {
    plan[[col]] <- deparse_lang_col(plan[[col]])
  }
  plan
}

deparse_lang_col <- function(x) {
  if (!length(x) || !is.list(x)) {
    return(x)
  }

  out <- unlist(lapply(x, safe_deparse, collapse = " ", backtick = TRUE))
  structure(out, class = "expr_list")
}

lang_cols <- function(plan) {
  intersect(colnames(plan), c("command", "dynamic", "trigger", "transform"))
}
