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

  err_reasons <- NULL

  if (length(missing_vars) > 0) {
    err_reasons <- glue::glue("Expected variables are missing: [{glue::glue_collapse(missing_vars, ', ')}]")
  }

  if (length(new_vars) > 0) {
    err_reasons <- c(err_reasons, paste0(
      paste0(
        glue::glue("Unexpected new variables: [{glue::glue_collapse(new_vars, ', ')}]."),
        "\n    Please edit documentation if this is intended."
      )
    ))
  }

  res <- TRUE

  if (length(missing_vars) > 0) {
    res <- FALSE
  }

  if (length(new_vars) > 0 && isTRUE(blueprint$stop_on_new_vars)) {
    res <- FALSE
  }

  if (!res) {
    return(fail_check(err_reasons))
  }

  warn_check(err_reasons)
}

#' @rdname checks
#' @export
all_types_match <- function(df, meta) {
  stopifnot(is.data.frame(df))
  stopifnot(inherits(meta, "blueprint_metadata"))

  df_types <- tidytable::tidytable(
    name = names(df),
    type = vcapply(df, typeof)
  )

  df_types <-
    df_types %>%
    tidytable::left_join(
      meta %>%
        tidytable::select("name", expected_type = "type"),
      by = "name"
    ) %>%
    tidytable::mutate(issue = .data$expected_type != .data$type)

  if (any(df_types$issue, na.rm = TRUE)) {
    format <- "{name}: '{type}' found, '{expected_type}' expected"

    df_types <-
      df_types %>%
      tidytable::mutate(.err = ifelse(
        .data$issue == TRUE,
        glue::glue(format),
        NA_character_
      ))

    errors <-
      df_types %>%
      tidytable::filter(.data$issue == TRUE) %>%
      tidytable::pull(.data$.err)

    return(fail_check(errors))
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
