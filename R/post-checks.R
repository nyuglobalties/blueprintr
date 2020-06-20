accept_content <- function(results, df, blueprint, meta) {
  if (has_dropped_feature(blueprint, meta)) {
    df <- drop_columns(df, blueprint, meta)
  }

  if (has_reorder_feature(blueprint, meta)) {
    df <- reorder_columns(df, blueprint, meta)
  }

  df
}

drop_columns <- function(df, blueprint, meta) {
  meta_dt <- as.data.table(meta)
  dropped_cols <- meta_dt[dropped == TRUE, dropped]

  if (length(dropped_cols) > 0) {
    if (is.data.table(df)) {
      df[, (dropped_cols) := NULL]
    } else {
      df <- df[, setdiff(names(df), dropped_cols)]
    }
  }

  df
}

reorder_columns <- function(df, blueprint, meta) {
  if (is.data.table(df)) {
    df[, meta$name, with = FALSE]
  } else {
    df[, meta$name]
  }
}
