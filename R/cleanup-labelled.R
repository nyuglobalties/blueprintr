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

  if (!requireNamespace("labelled", quietly = TRUE)) {
    bp_err("The labelled package is needed to create labelled columns")
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

  if (!is.null(var_title)) {
    names(var_title) <- variable
    arg_list <- c(list(df), as.list(var_title))

    df <- do.call(labelled::set_variable_labels, arg_list)
  }

  if (!is.null(haven_labels)) {
    labels <- list(haven_labels)
    names(labels) <- variable
    arg_list <- c(list(df), labels)

    df <- do.call(labelled::set_value_labels, arg_list)
  }

  df
}
