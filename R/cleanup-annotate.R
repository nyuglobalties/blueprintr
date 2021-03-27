has_annotation_cleanup <- function(bp) {
  isTRUE(bp$annotate)
}

annotate_variables <- function(df, bp, meta) {
  overwrite <- bp$annotate_overwrite

  for (vn in names(df)) {
    df[[vn]] <- annotate_variable(df[[vn]], vn, meta, overwrite)
  }

  df
}

annotate_variable <- function(x, varname, meta, overwrite) {
  meta <- dplyr::filter(meta, .data$name == varname)
  fields <- setdiff(names(meta), c("name", "type", "dropped"))

  for (f in fields) {
    x <- add_annotation(x, f, meta[[f]], overwrite)
  }

  x
}