#' @export 
mutate_annotation <- function(.data, .field, ..., .overwrite = TRUE) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.character(.field), length(.field) == 1)

  queries <- rlang::enquos(...)

  for (var in names(queries)) {
    if (!var %in% names(.data)) {
      bp_err(c(
        "'{var}' not found in `{substitute(.data)}`. ",
        "Cannot modify meta decoration '{.field}' on it."
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