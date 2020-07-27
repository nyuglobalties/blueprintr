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
  # Extended cleanup features
  if (has_labelled_feature(blueprint)) {
    df <- label_columns(df, blueprint, meta)
  }

  # Default cleanup features
  if (has_reorder_feature(blueprint, meta)) {
    df <- reorder_columns(df, blueprint, meta)
  }

  if (has_dropped_feature(blueprint, meta)) {
    df <- drop_columns(df, blueprint, meta)
  }

  df
}
