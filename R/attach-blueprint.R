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
  bp_assert(inherits(plan, "drake_plan"))

  UseMethod("attach_blueprint", blueprint)
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

