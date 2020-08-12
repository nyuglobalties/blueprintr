#' Blueprint drake target names
#'
#' All blueprint associated target names:
#'   * `{blueprint}_draft`: The result of running the blueprint's command
#'   * `{blueprint}_info`: A list structure that includes a reference to the
#'                         blueprint itself, the metadata, and the check results
#'   * `{blueprint}`: The built dataset after cleanup
#'   * `{blueprint}_codebook`: Optional step that renders a codebook
#'
#' @param x A blueprint or string relating to a blueprint's name
#' @param ... Unused for now
#'
#' @export
blueprint_target_name <- function(x, ...) {
  UseMethod("blueprint_target_name")
}

#' @rdname blueprint_target_name
#' @export
blueprint_target_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @rdname blueprint_target_name
#' @export
blueprint_target_name.character <- function(x, ...) {
  x
}

#' @rdname blueprint_target_name
#' @export
blueprint_final_name.blueprint <- function(x, ...) {
  blueprint_final_name(x$name)
}

#' @rdname blueprint_target_name
#' @export
blueprint_draft_name <- function(x, ...) {
  UseMethod("blueprint_draft_name")
}

#' @rdname blueprint_target_name
#' @export
blueprint_draft_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @rdname blueprint_target_name
#' @export
blueprint_draft_name.character <- function(x, ...) {
  paste0(blueprint_target_name(x), "_draft")
}

#' @rdname blueprint_target_name
#' @export
blueprint_draft_name.blueprint <- function(x, ...) {
  blueprint_draft_name(x$name)
}

#' @rdname blueprint_target_name
#' @export
blueprint_info_name <- function(x, ...) {
  UseMethod("blueprint_info_name")
}

#' @rdname blueprint_target_name
#' @export
blueprint_info_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @rdname blueprint_target_name
#' @export
blueprint_info_name.character <- function(x, ...) {
  paste0(blueprint_target_name(x), "_info")
}

#' @rdname blueprint_target_name
#' @export
blueprint_info_name.blueprint <- function(x, ...) {
  blueprint_info_name(x$name)
}

#' @rdname blueprint_target_name
#' @export
blueprint_codebook_name <- function(x, ...) {
  UseMethod("blueprint_codebook_name")
}

#' @rdname blueprint_target_name
#' @export
blueprint_codebook_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @rdname blueprint_target_name
#' @export
blueprint_codebook_name.character <- function(x, ...) {
  paste0(blueprint_target_name(x), "_codebook")
}

#' @rdname blueprint_target_name
#' @export
blueprint_codebook_name.blueprint <- function(x, ...) {
  blueprint_codebook_name(x$name)
}
