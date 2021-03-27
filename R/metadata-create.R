#' Create a metadata file from a dataset
#'
#' One of the targets in the blueprint drake target chain.
#' If a metadata file does not exist and the blueprint has
#' `export_metadata` set to `TRUE`, then this function will be
#' added to the drake plan during the blueprint attaching step.
#' In case the metadata file is accidentally deleted, the `.file`
#' paramter is set to `file_out(metadata_path(blueprint))`
#' so that drake can monitor the metadata file's state.
#'
#' @param df A dataframe that the metadata table describes
#' @param blueprint The original blueprint for the dataframe
#' @param ... A variable list of metadata tables on which this
#'            metadata table depends
#' @export
create_metadata_file <- function(df, blueprint, ...) {
  stopifnot(is.data.frame(df))

  metadata_dt <- initial_metadata_dt(df)
  deps_metalist <- dots_list(...)

  metadata_dt <- propagate_metadata(metadata_dt, df, deps_metalist)

  # panelcleaner interop
  if (isTRUE(blueprint$import_mdf_meta)) {
    metadata_dt <- mdf_import_meta(metadata_dt, df)
  }

  if (!file.exists(metadata_path(blueprint))) {
    handle_write_meta_file(metadata_dt, blueprint)
  }

  metadata_path(blueprint)
}

initial_metadata_dt <- function(df) {
  dplyr::tibble(
    name = names(df),
    description = NA_character_,
    type = vcapply(df, typeof)
  )
}

propagate_metadata <- function(metadata_dt, df, deps_metalist) {
  decs_dt <- link_annotation_meta(metadata_dt, df)

  if (length(deps_metalist) > 0) {
    metafile_dt <- link_dependency_meta(metadata_dt, deps_metalist)
    metadata_dt <- reconcile_dependencies(decs_dt, metafile_dt)
  } else {
    metadata_dt <- decs_dt
  }

  metadata_dt
}

link_dependency_meta <- function(meta_dt, deps_metalist) {
  meta_dt <- dplyr::select(meta_dt, .data$name, .data$type)

  deps_meta_full <- dplyr::bind_rows(!!!deps_metalist)
  deps_meta_full <- dplyr::rename(
    deps_meta_full,
    deps_type = .data$type
  )

  meta_dt <- dplyr::left_join(meta_dt, deps_meta_full, by = "name")
  meta_dt <- dplyr::mutate(
    meta_dt,
    .origin = "metafile"
  )

  # Metadata-file-based linking has the potential to have multiple
  # entries per variable, so entries need to be aggregated based on
  # *unique*, non-NA values
  fields <- setdiff(names(meta_dt), "name")

  meta_dt <- dplyr::group_by(meta_dt, .data$name)
  meta_dt <- dplyr::summarise(
    meta_dt,
    dplyr::across(
      .cols = dplyr::all_of(fields),
      .fns = function(x) {
        paste0(unique_val(x), collapse = "|")
      }
    ),
    .groups = "drop"
  )

  meta_dt
}

link_annotation_meta <- function(meta_dt, df) {
  meta_dt <- dplyr::select(meta_dt, .data$name, .data$type)
  meta_dt <- dplyr::left_join(
    meta_dt,
    annotation_table_df(df)
  )
  dplyr::mutate(
    meta_dt,
    .origin = "annotations"
  )
}

reconcile_dependencies <- function(dec_dt, meta_dt) {
  all_meta <- dplyr::bind_rows(dec_dt, meta_dt)
  fields <- setdiff(names(all_meta), c("name", ".origin"))

  wide_meta <- tidyr::pivot_wider(
    all_meta,
    names_from = .data$.origin,
    values_from = dplyr::all_of(fields)
  )

  # Remove rows with NA type_annotations
  # These don't exist in the dataset
  wide_meta <- dplyr::filter(
    wide_meta,
    !is.na(.data$type_annotations)
  )

  wide_meta <- dplyr::rename(
    wide_meta,
    type = .data$type_annotations
  )

  wide_meta[["type_metafile"]] <- NULL

  custom_fields <- setdiff(fields, "type")

  for (field in custom_fields) {
    decs <- glue("{field}_annotations")
    mets <- glue("{field}_metafile")

    wide_meta[[field]] <- dplyr::coalesce(
      wide_meta[[decs]],
      wide_meta[[mets]]
    )

    wide_meta <- dplyr::relocate(
      wide_meta,
      .data[[field]],
      .before = .data[[decs]]
    )

    wide_meta[[decs]] <- NULL
    wide_meta[[mets]] <- NULL
  }

  wide_meta
}

handle_write_meta_file <- function(metadata_dt, bp) {
  if ("deps_type" %in% names(metadata_dt)) {
    metadata_dt <- dplyr::mutate(metadata_dt, deps_type = NULL)
  }

  write_meta_file(metadata_dt, metadata_path(bp))
  invisible(metadata_dt)
}

write_meta_file <- function(x, path) {
  x <- remove_parsed_tests(x)
  readr::write_csv(x, path, na = "")

  invisible(x)
}

remove_parsed_tests <- function(x) {
  if (".parsed_tests" %in% names(x)) {
    x <- dplyr::select(x, -.data$.parsed_tests)
  }

  x
}
