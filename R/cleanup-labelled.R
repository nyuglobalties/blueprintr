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

  # If data.table, downconvert to data.frame but preserve blueprintr attributes
  df <- preserve_blueprintr_attrs(df, as.data.frame)

  # Add evaluated codings to meta
  meta <- tidytable::mutate(meta, .evaluated_coding = string_to_coding(.data$coding))

  for (variable in meta$name) {
    df[[variable]] <- label_column(df[[variable]], variable, meta)
  }

  preserve_blueprintr_attrs(df, as.data.frame)
}

label_column <- function(x, nx, meta) {
  meta_subset <- tidytable::filter(meta, .data$name == nx)

  if ("title" %in% names(meta_subset)) {
    var_title <- meta_subset[["title"]]
  } else {
    var_title <- meta_subset[["description"]]
  }

  if (var_title == "" || is.na(var_title)) {
    var_title <- NULL
  }

  var_coding <- meta_subset[[".evaluated_coding"]][[1]]
  haven_labels <- rcoder::coding_to_haven_labels(var_coding)

  if (!is.null(var_title)) {
    x <- attr_safe(x, function(.x) {
      labelled::var_label(.x) <- var_title
      .x
    }, .p = function(.nx) grepl("^(bpr\\.|\\.uuid)", .nx))
  }

  if (!is.null(haven_labels)) {
    x <- attr_safe(x, function(.x) {
      labelled::val_labels(.x) <- haven_labels
      .x
    }, .p = function(.nx) grepl("^(bpr\\.|\\.uuid)", .nx))
  }

  x
}
