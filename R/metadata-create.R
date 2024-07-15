#' Create a metadata file from a dataset
#'
#' One of the targets in the blueprint workflow target chain.
#' If a metadata file does not exist, then this function will be
#' added to the workflow.
#'
#' @param df A dataframe that the metadata table describes
#' @param blueprint The original blueprint for the dataframe
#' @param ... A variable list of metadata tables on which this
#'            metadata table depends
#' @export
create_metadata_file <- function(df, blueprint, ...) {
  stopifnot(is.data.frame(df))

  metadata_dt <- initial_metadata_dt(df)
  deps_metalist <- rlang::dots_list(...)

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
  tidytable::tidytable(
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

  # Remove .origin if no reconciliation happened
  if (".origin" %in% names(metadata_dt)) {
    metadata_dt[[".origin"]] <- NULL
  }

  # Ensure "description" is available for metadata reqs
  if (!"description" %in% names(metadata_dt)) {
    metadata_dt[["description"]] <- NA_character_
  }

  metadata_dt
}

link_dependency_meta <- function(meta_dt, deps_metalist) {
  meta_dt <- tidytable::select(meta_dt, tidyselect::all_of(c("name", "type")))

  deps_meta_full <- tidytable::bind_rows(deps_metalist)
  deps_meta_full <- tidytable::rename(
    deps_meta_full,
    deps_type = "type"
  )

  meta_dt <- tidytable::left_join(meta_dt, deps_meta_full, by = "name", multiple = "all")
  meta_dt <- tidytable::mutate(
    meta_dt,
    .origin = "metafile"
  )

  # Metadata-file-based linking has the potential to have multiple
  # entries per variable, so entries need to be aggregated based on
  # *unique*, non-NA values
  fields <- setdiff(names(meta_dt), "name")

  meta_dt <- tidytable::summarise(
    meta_dt,
    tidytable::across(
      .cols = tidyselect::all_of(fields),
      .fns = function(x) {
        paste0(unique_val(x), collapse = "|")
      }
    ),
    .by = "name",
    .sort = FALSE
  )

  meta_dt
}

link_annotation_meta <- function(meta_dt, df) {
  meta_dt <- tidytable::select(meta_dt, tidyselect::all_of(c("name", "type")))
  meta_dt <- tidytable::left_join(
    meta_dt,
    annotation_table_df(df),
    by = "name"
  )
  tidytable::mutate(
    meta_dt,
    .origin = "annotations"
  )
}

reconcile_dependencies <- function(dec_dt, meta_dt) {
  # Don't include internal fields
  accepted_vars <- unique(c(names(dec_dt), names(meta_dt)))
  accepted_vars <- accepted_vars[!grepl("^\\.", accepted_vars)]
  accepted_vars <- c(accepted_vars, ".origin")

  dec_dt <- tidytable::select(
    dec_dt,
    tidytable::any_of(accepted_vars)
  )

  meta_dt <- tidytable::select(
    meta_dt,
    tidytable::any_of(accepted_vars)
  )

  all_meta <- tidytable::bind_rows(dec_dt, meta_dt)

  # As of tidytable 0.11.0, pivoting affects the row order.
  # While analytically this may be fine, this messes up metadata creation.
  # These row IDs return the data to its original order.
  all_meta <- tidytable::mutate(
    all_meta,
    .id = seq_len(.N),
  )
  fields <- setdiff(names(all_meta), c("name", ".origin"))

  wide_meta <- tidytable::pivot_wider(
    all_meta,
    names_from = ".origin",
    values_from = tidytable::all_of(fields)
  ) |>
    tidytable::mutate(
      .id = tidytable::coalesce(
        .id_annotations,
        .id_metafile,
      )
    ) |>
    tidytable::arrange(.id) |>
    tidytable::select(-tidyselect::starts_with(".id"))

  # Remove rows with NA type_annotations
  # These don't exist in the dataset
  wide_meta <- tidytable::filter(
    wide_meta,
    !is.na(.data$type_annotations)
  )

  wide_meta <- tidytable::rename(
    wide_meta,
    type = "type_annotations"
  )

  wide_meta[["type_metafile"]] <- NULL

  custom_fields <- setdiff(fields, "type")

  for (field in custom_fields) {
    decs <- glue::glue("{field}_annotations")
    mets <- glue::glue("{field}_metafile")

    wide_meta[[field]] <- tidytable::coalesce(
      wide_meta[[decs]],
      wide_meta[[mets]]
    )

    wide_meta <- tidytable::relocate(
      wide_meta,
      tidyselect::all_of(field),
      .before = tidyselect::all_of(decs)
    )

    wide_meta[[decs]] <- NULL
    wide_meta[[mets]] <- NULL
  }

  wide_meta
}

handle_write_meta_file <- function(metadata_dt, bp) {
  if ("deps_type" %in% names(metadata_dt)) {
    metadata_dt <- tidytable::mutate(metadata_dt, deps_type = NULL)
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
    x <- tidytable::select(x, -.data$.parsed_tests)
  }

  x
}
