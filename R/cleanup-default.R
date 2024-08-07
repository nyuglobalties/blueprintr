has_dropped_feature <- function(blueprint, meta) {
  "dropped" %in% names(meta) &&
    !identical(blueprint$drop_columns, FALSE)
}

drop_columns <- function(df, blueprint, meta) {
  dropped_cols <- meta[!is.na(meta$dropped) & meta$dropped == TRUE, "name", drop = TRUE]

  if (length(dropped_cols) > 0) {
    df <- tidytable::select(df, -tidyselect::all_of(dropped_cols))
  }

  df
}

has_reorder_feature <- function(blueprint, meta) {
  !identical(blueprint$reorder_columns, FALSE)
}

reorder_columns <- function(df, blueprint, meta) {
  if (tidytable::is_tidytable(df) || data.table::is.data.table(df)) {
    df <- as.data.frame(df)
  }

  df[, meta$name]
}
