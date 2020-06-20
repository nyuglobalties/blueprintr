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
      glue("Unexpected new variables: {glue_collapse(new_vars, ', ')}"),
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

all_types_match <- function(df, meta) {
  stopifnot(is.data.frame(df))
  stopifnot(inherits(meta, "blueprint_metadata"))

  df_types <- data.table(
    name = names(df),
    type = vcapply(df, typeof)
  )

  meta_dt <- as.data.table(meta)

  df_types[, expected_type := meta_dt[.SD, type, on = "name"]]
  df_types[, issue := expected_type != type]

  if (any(df_types$issue, na.rm = TRUE)) {
    format <- "{name}: '{type}' found, '{expected_type}' expected"
    df_types[issue == TRUE, .err := glue(format)]

    for (.err in df_types[issue == TRUE, .err]) {
      message(.err)
    }

    return(FALSE)
  }
  
  TRUE
}
