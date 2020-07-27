has_labelled_feature <- function(blueprint) {
  if (is.null(blueprint$labelled)) {
    return(FALSE)
  }

  isTRUE(blueprint$labelled)
}

label_columns <- function(df, blueprint, meta) {
  if (!requireNamespace("rcoder", quietly = TRUE)) {
    bp_err("The rcoder package is needed to create labelled columns")
  }

  if (!requireNamespace("haven", quietly = TRUE)) {
    bp_err("The haven package is needed to create labelled columns")
  }

  if (!"coding" %in% names(meta)) {
    bp_err("`coding` is not available in {ui_value(blueprint$name)} metadata")
  }

  # If data.table, downconvert to data.frame
  df <- as.data.frame(df)

  # Add evaluated codings to meta
  meta <- dplyr::mutate(meta, .evaluated_coding = string_to_coding(.data$coding))

  for (variable in meta$name) {
    df <- label_column(variable, df, meta)
  }

  df
}

label_column <- function(variable, df, meta) {
  if ("title" %in% names(meta)) {
    var_title <- meta[meta$name == variable, "title", drop = TRUE]
  } else {
    var_title <- meta[meta$name == variable, "description", drop = TRUE]
  }

  if (var_title == "" || is.na(var_title)) {
    var_title <- NULL
  }

  var_coding <- meta[meta$name == variable, ][[".evaluated_coding"]][[1]]
  haven_labels <- rcoder::coding_to_haven_labels(var_coding)

  if (is.null(var_title) && is.null(haven_labels)) {
    return(df)
  }

  df[[variable]] <- haven::labelled(
    df[[variable]],
    labels = haven_labels,
    label = var_title
  )

  df
}
