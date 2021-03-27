#' Convert an input dataframe into a metadata object
#'
#' @param df A dataframe that will be converted into a
#'           metadata object, once content checks pass.
#' @export
metadata <- function(df) {
  stopifnot(is.data.frame(df))

  # Metadata MUST have name, description, and type columns at least
  req_columns <- c("name", "description", "type")
  if (any(!req_columns %in% names(df))) {
    missing_columns <- req_columns[!req_columns %in% names(df)]

    bp_err(c(
      "Required blueprint metadata columns",
      collapse_message_list(missing_columns),
      "not found."
    ))
  }

  if ("tests" %in% names(df) && !is.list(df$tests)) {
    df <- dplyr::mutate(df, .parsed_tests = parse_tests(.data$tests))
  }

  structure(
    df,
    class = c("blueprint_metadata", class(df))
  )
}

parse_variable_tests <- function(x) {
  stopifnot(length(x) == 1)

  if (is.na(x)) {
    x <- ""
  }

  x <- glue("check_list({x})")

  rlang::eval_bare(rlang::parse_expr(x))
}

parse_tests <- function(x) {
  stopifnot(is.character(x) || all(is.na(x)))

  lapply(x, parse_variable_tests)
}

metadata_path <- function(blueprint) {
  blueprint$metadata_file_path
}

metadata_file_exists <- function(blueprint) {
  file.exists(metadata_path(blueprint))
}

load_metadata <- function(blueprint) {
  if (!metadata_file_exists(blueprint)) {
    bp_err("No metadata exists to load for {blueprint$name}")
  }


  read_metadata(metadata_path(blueprint))
}

#' Read the metadata file
#'
#' Reads the metadata from, found at a given path, and returns a `metadata()`
#' object. This is mostly an internal function for the blueprint steps.
#'
#' @param metadata_file Path of metadata file. Currently only supports CSVs
#' @return A `metadata()` object
#'
#' @export
#' @keywords internal
read_metadata <- function(metadata_file) {
  metadata_df <-
    readr::read_csv(
      metadata_file,
      col_types = readr::cols()
    )

  metadata(metadata_df)
}

metadata_target_name <- function(blueprint) {
  paste0(blueprint_final_name(blueprint), "_meta")
}

metadata_target_file_name <- function(blueprint) {
  paste0(blueprint_final_name(blueprint), "_meta_path")
}

metadata_export_name <- function(blueprint) {
  paste0(blueprint_final_name(blueprint), "_metadata_export")
}