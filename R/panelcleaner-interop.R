mdf_import_meta <- function(metadata_dt, mapped_df) {
  if (!inherits(mapped_df, "mapped_df")) {
    return(metadata_dt)
  }

  mapping_subset <- mdfm_keep_panel(mapped_df)

  mapping_subset <- tidytable::select(
    mapping_subset,
    name = !!as.name(mdf_schema_homogenized_name(mapped_df)),
    pm_coding = !!as.name(mdf_schema_homogenized_coding(mapped_df))
  )

  metadata_dt <- tidytable::left_join(
    metadata_dt,
    mapping_subset,
    by = "name"
  )

  coalesce_pm_coding_cols(as.data.frame(metadata_dt))
}

mdfm_keep_panel <- function(mapped_df) {
  pnls_panel <- mdf_schema_panel(mapped_df)
  mapping <- mdf_panel_mapping(mapped_df)

  using <- mapping[!is.na(mapping[[pnls_panel]]), ]
  using[using[[pnls_panel]] == mdf_panel_name(mapped_df), ]
}

mdf_panel_name <- function(mapped_df) {
  get_attr(mapped_df, "panel_name")
}

mdf_panel_mapping <- function(mapped_df) {
  get_attr(mapped_df, "mapping")
}

mdf_mapping_schema <- function(mapped_df) {
  get_attr(mdf_panel_mapping(mapped_df), "schema")
}

mdf_schema_panel <- function(mapped_df) {
  mdf_mapping_schema(mapped_df)$panel
}

mdf_schema_homogenized_name <- function(mapped_df) {
  mdf_mapping_schema(mapped_df)$homogenized_name
}

mdf_schema_homogenized_coding <- function(mapped_df) {
  mdf_mapping_schema(mapped_df)$homogenized_coding
}

coalesce_pm_coding_cols <- function(df) {
  stopifnot("pm_coding" %in% names(df))

  if ("coding" %in% names(df)) {
    df$coding <- tidytable::coalesce(df$pm_coding, df$coding)
  } else {
    df$coding <- df$pm_coding
  }

  df$pm_coding <- NULL
  df
}
