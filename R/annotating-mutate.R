#' Modify dataset variable annotations
#' 
#' Usually, metadata should be a reflection of what the data
#' *should* represent and act as a check on the generation code.
#' However, in the course of data aggregation, it can be common to 
#' perform massive transformations that would be cumbersome to 
#' document manually. This exposes a metadata-manipulation framework
#' prior to metadata file creation, in the style of `dplyr::mutate`.
#' 
#' @param .data A `data.frame`
#' @param .field The name of the annotation field that you wish to modify
#' @param ... 
#'   * For `mutate_annotation`, named parameters that contain the annotation 
#'     values. Like `dplyr::mutate`, each parameter name is a variable 
#'     (that must already exist!), and each parameter value is an R expression, 
#'     evaluated with `.data` as a data mask.
#'   * For `mutate_annotation_across`, extra arguments passed to `.fn`
#' @param .fn A function that takes in a vector, the currently selected variable
#' @param .cols A tidyselect-compatible selection of variables to be edited
#' @param .overwrite If `TRUE`, overwrites existing annotation values.
#'   Annotations have an overwriting guard by default, but since these functions
#'   are intentionally modifying the annotations, this parameter 
#'   defaults to `TRUE`.
#' @return A `data.frame` with annotated columns
#' 
#' @name mutate_annotation
#' @export
NULL

#' @rdname mutate_annotation
#' @export
mutate_annotation <- function(.data, .field, ..., .overwrite = TRUE) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.character(.field), length(.field) == 1)

  queries <- rlang::enquos(...)

  for (var in names(queries)) {
    if (!var %in% names(.data)) {
      bp_err(c(
        "'{var}' not found in `{substitute(.data)}`. ",
        "Cannot modify annotation '{.field}' on it."
      ))
    }

    evald <- rlang::eval_tidy(
      queries[[var]],
      data = .data
    )

    .data[[var]] <- add_annotation(
      .data[[var]],
      .field,
      evald,
      overwrite = .overwrite
    )
  }

  .data
}

#' @rdname mutate_annotation
#' @export
mutate_annotation_across <- function(
  .data, 
  .field,
  .fn,
  .cols = dplyr::everything(),
  ...,
  .overwrite = TRUE
) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.character(.field), length(.field) == 1)
  stopifnot(is.function(.fn))

  vars <- names(dplyr::select(.data, {{ .cols }}))

  for (var in vars) {
    evald <- .fn(.data[[var]], ...)

    .data[[var]] <- add_annotation(
      .data[[var]],
      .field,
      evald,
      overwrite = .overwrite
    )
  }

  .data
}