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
  dropped_cols <- 
    meta %>% 
    dplyr::filter(.data$dropped == TRUE) %>% 
    dplyr::pull(.data$name)

  if (length(dropped_cols) > 0) {
    df <- dplyr::select(df, -dropped_cols)
  }

  df
}

reorder_columns <- function(df, blueprint, meta) {
  dplyr::select(df, meta$name)
}
