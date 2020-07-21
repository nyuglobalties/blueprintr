#' Run clean-up tasks and return built dataset
#'
#' After checks pass, this step runs in the blueprint sequence.
#' If any cleanup features are enabled, they will run on the
#' dataset prior to setting the final blueprint target.
#'
#' @param results A reference to the checks results. Currently used to
#'                ensure that this step runs after the checks step.
#' @param df The built dataset
#' @param blueprint The blueprint associated with the built dataset
#' @param meta The metadata associated with the built dataset
#'
#' @export
accept_content <- function(results, df, blueprint, meta) {
  if (has_reorder_feature(blueprint, meta)) {
    df <- reorder_columns(df, blueprint, meta)
  }

  if (has_dropped_feature(blueprint, meta)) {
    df <- drop_columns(df, blueprint, meta)
  }

  df
}

drop_columns <- function(df, blueprint, meta) {
  dropped_cols <- meta[!is.na(meta$dropped) & meta$dropped == TRUE, "name", drop = TRUE]

  if (length(dropped_cols) > 0) {
    df <- dplyr::select(df, -dropped_cols)
  }

  df
}

reorder_columns <- function(df, blueprint, meta) {
  df[, meta$name]
}
