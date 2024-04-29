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
  dots <- rlang::dots_list(...)

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

  out <- as.data.frame(tidytable::bind_rows(plan, bp_plan))

  structure(
    out,
    class = c("drake_plan", class(out))
  )
}

blueprint_plan <- function(bp) {
  steps <- assembly_steps(drake_assembler(), bp)

  tidytable::bind_rows(lapply(steps, function(step) step$built_payload))
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
