#' Evaluate checks on the blueprint build output
#'
#' After building a dataset, it's beneficial (if not
#' a requirement) to run tests on that dataset to ensure
#' that it behaves as expected. `blueprintr` gives authors
#' a framework to run these tests automatically, both for
#' individual variables and general dataset checks.
#' `blueprintr` provides three functions as models for developing
#' these kinds of functions: one to check that all expected variables
#' are present, one to check the variable types, and a generic
#' function that checks if variable values are contained within
#' a known set.
#'
#' @param df The built dataset
#' @param meta The dataset's metadata
#' @param blueprint The dataset's blueprint
#'
#' @name checks
NULL

#' @rdname checks
#' @export
all_variables_present <- function(df, meta, blueprint) {
  stopifnot(is.data.frame(df))
  stopifnot(inherits(meta, "blueprint_metadata"))

  known_vars <- meta$name

  new_vars <- setdiff(names(df), known_vars)
  missing_vars <- setdiff(known_vars, names(df))

  if (length(missing_vars) > 0) {
    message(
      glue("Expected variables are missing: {glue_collapse(missing_vars, ', ')}")
    )
  }

  if (length(new_vars) > 0) {
    message(
      glue("Unexpected new variables: [{glue_collapse(new_vars, ', ')}]. "),
      "Please edit documentation if this is intended."
    )
  }

  if (length(missing_vars) > 0) {
    return(FALSE)
  }

  if (length(new_vars) > 0 && isTRUE(blueprint$stop_on_new_vars)) {
    return(FALSE)
  }

  TRUE
}

#' @rdname checks
#' @export
all_types_match <- function(df, meta) {
  stopifnot(is.data.frame(df))
  stopifnot(inherits(meta, "blueprint_metadata"))

  df_types <- dplyr::tibble(
    name = names(df),
    type = vcapply(df, typeof)
  )

  df_types <-
    df_types %>%
    dplyr::left_join(
      meta %>%
        dplyr::select(.data$name, expected_type = .data$type),
      by = "name"
    ) %>%
    dplyr::mutate(issue = .data$expected_type != .data$type)

  if (any(df_types$issue, na.rm = TRUE)) {
    format <- "{name}: '{type}' found, '{expected_type}' expected"

    df_types <-
      df_types %>%
      dplyr::mutate(.err = ifelse(
        .data$issue == TRUE,
        glue(.data$format),
        NA_character_
      ))

    errors <-
      df_types %>%
      dplyr::filter(.data$issue == TRUE) %>%
      dplyr::pull(.data$.err)

    for (.err in errors) {
      message(.err)
    }

    return(FALSE)
  }

  TRUE
}

#' Test if x is a subset of y
#'
#' @param x A vector
#' @param y A vector representing an entire set
#' @export
in_set <- function(x, y) {
  all(unique(x) %in% y)
}
