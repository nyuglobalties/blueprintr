has_dropped_feature <- function(blueprint, meta) {
  "dropped" %in% names(meta) &&
    !identical(blueprint$drop_columns, FALSE)
}

has_reorder_feature <- function(blueprint, meta) {
  !identical(blueprint$reorder_columns, FALSE)
}
