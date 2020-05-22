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

  structure(
    df,
    class = c("blueprint_metadata", class(df))
  )
}

clean_tests_column <- function(x) {
  if (is.character(x)) {

  }
}

metadata_path <- function(blueprint) {
  file.path(
    blueprint$metadata_file_path, 
    glue("{blueprint$name}.{blueprint$metadata_file_type}")
  )
}

metadata_file_exists <- function(blueprint) {
  file.exists(metadata_path(blueprint))
}

create_metadata_file <- function(df, blueprint) {
  stopifnot(is.data.frame(df))

  metadata_dt <- data.table(name = names(df), description = "", type = vcapply(df, typeof))

  data.table::fwrite(df, file = metadata_path(blueprint))
}

load_metadata <- function(blueprint) {
  if (!metadata_file_exists(blueprint)) {
    bp_err("No metadata exists to load for {blueprint$name}")
  }

  metadata_df <- as.data.frame(data.table::fread(metadata_path(blueprint)))

  metadata(metadata_df)
}

metadata_target_name <- function(blueprint) {
  paste0(blueprint_target_name(blueprint), "_meta")
}
