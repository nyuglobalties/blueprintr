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
    df$tests <- I(parse_tests(df$tests))
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
  stopifnot(is.character(x))

  lapply(x, parse_variable_tests)
}

metadata_path <- function(blueprint) {
  blueprint$metadata_file_path
}

metadata_file_exists <- function(blueprint) {
  file.exists(metadata_path(blueprint))
}

#' @export
create_metadata_file <- function(df, blueprint, ...) {
  stopifnot(is.data.frame(df))

  metadata_dt <- dplyr::tibble(
    name = names(df), 
    description = NA_character_, 
    type = vcapply(df, typeof)
  )

  deps_metalist <- dots_list(...)

  if (length(deps_metalist) > 0) {
    metadata_dt <- link_dependency_meta(metadata_dt, deps_metalist)
  }

  if (any(metadata_dt$type_issue, na.rm = TRUE)) {
    data.table::fwrite(metadata_dt, file = metadata_path(blueprint))

    bp_err(c(
      "Type inconsistency between current and previous variables.\n",
      "Please edit the metadata file to resolve the issue and then rerun."
    ))
  }

  metadata_dt <- 
    metadata_dt %>% 
    dplyr::mutate(type_issue = NULL, deps_type = NULL)

  data.table::fwrite(metadata_dt, file = metadata_path(blueprint))

  if (any(duplicated(metadata_dt$name))) {
    bp_err(c(
      "Metadata for {ui_value(blueprint$name)} has duplicated variables.\n",
      "This can happen if a variable in a dataset exists in multiple depencies.\n",
      "Please edit the metadata file to resolve the issue and then rerun."
    ))
  }

  metadata(metadata_dt)
}

link_dependency_meta <- function(meta_dt, deps_metalist) {
  meta_dt <-
    meta_dt %>% 
    dplyr::select(.data$name, .data$type)

  deps_meta_full <- 
    dplyr::bind_rows(!!!deps_metalist) %>% 
    dplyr::rename(deps_type = .data$type)

  meta_dt <-
    meta_dt %>% 
    dplyr::left_join(deps_meta_full, by = "name") %>% 
    dplyr::mutate(type_issue = .data$type != .data$deps_type)

  meta_dt
}

load_metadata <- function(blueprint) {
  if (!metadata_file_exists(blueprint)) {
    bp_err("No metadata exists to load for {blueprint$name}")
  }

  metadata_df <- as.data.frame(data.table::fread(metadata_path(blueprint)))

  metadata(metadata_df)
}

metadata_target_name <- function(blueprint) {
  paste0(blueprint_final_name(blueprint), "_meta")
}

metadata_export_name <- function(blueprint) {
  paste0(blueprint_final_name(blueprint), "_metadata_export")
}
