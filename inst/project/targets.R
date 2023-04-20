#' Fetch a file from the OSF
#'
#' @param target_name A symbol to be used as the target name
#' @param guid The GUID of the asset on the OSF
#' @param ... Arguments passed to [targets::tar_target_raw]
#' @return A target that uses [osfutils::osfcache_get] to leverage
#'   a caching strategy for assets on the OSF
tar_osffile <- function(target_name, guid, ...) {
  stopifnot(is.character(guid))
  tarname <- as.character(substitute(target_name))

  targets::tar_target_raw(
    tarname,
    bquote(osfutils::osfcache_get(.(guid), conflicts = "overwrite")),
    cue = tarchetypes::tar_cue_force(osfutils::osfcache_is_outdated(guid)),
    ...
  )
}

#' Get a dataset from KoBo with tidied variable names
#'
#' @param target_name A symbol to be used as the target name
#' @param asset_id The ID of the form on KoBo
#' @param missing_vars What sort of message happens if variables expected in
#'   the survey form are missing from the data. Passed to [karpi::kpi_get_data]
#' @param system_vars A character vector of which ODK system variables are kept
#'   in the data. Passed to [karpi::kpi_get_data]
#' @param verbose If `TRUE`, logs messages about data being fetched from KoBo
#' @param ... Arguments passed to [targets::tar_target_raw]
#' @return A data.frame with tidied variable names from KoBo
tar_kpiget_dekoboed <- function(target_name,
                                asset_id,
                                missing_vars = "msg",
                                system_vars = c("_id", "_uuid"),
                                verbose = TRUE,
                                datalad_backup = FALSE,
                                datalad_dir = datalad_proj_raw_path(),
                                ...) {
  stopifnot(is.character(asset_id))
  tarsym <- substitute(target_name)
  tarname <- as.character(tarsym)

  out <- list(
    targets::tar_target_raw(
      tarname,
      bquote(dekoboify(kpi_get(
        .(asset_id),
        missing_vars = .(missing_vars),
        system_vars = .(system_vars),
        .verbose = .(verbose)
      ))),
      cue = tar_cue_force(kpicache_is_outdated(asset_id)),
      ...
    )
  )

  if (isTRUE(datalad_backup)) {
    datalad_dir_sub <- substitute(datalad_dir)

    out[[2]] <- targets::tar_target_raw(
      paste0(tarname, "_box_file"),
      bquote({
        backup_dir <- .(datalad_dir_sub)
        message("Backing up '", .(tarname), "' to '", backup_dir, "'")

        path <- file.path(backup_dir, paste0(.(tarname), ".parquet"))
        arrow::write_parquet(.(tarsym), sink = path)
        path
      }),
      format = "file"
    )
  }

  out
}

#' Get a sheet from Google Sheets
#'
#' Downloads a sheet from Google Sheets, if it is outdated. If not
#' a cached version will be returned.
#'
#' @param tarsym A symbol to be used as the target name
#' @param url The URL of the Google Sheet
#' @param sheet If not `NULL`, the name of the sheet
#' @param range If not `NULL`, the address of a range to return.
#'   Preferred over `sheet`.
#' @param ... Extra arguments forwarded to `cache_get()`
#' @return A collection of targets to be added to a targets pipeline
tar_gsheet <- function(tarsym,
                       url,
                       sheet = NULL,
                       range = NULL,
                       ...) {
  tarname <- as.character(substitute(tarsym))
  tarname_file <- paste0(tarname, "_file")
  urlsym <- substitute(url)

  asset_id <- bquote(list(
    url = .(urlsym),
    sheet = .(sheet),
    range = .(range)
  ))

  getcall <- rlang::call2(
    "cache_get",
    quote(gscache()),
    asset_id,
    ...
  )

  list(
    targets::tar_target_raw(
      tarname_file,
      getcall,
      cue = tarchetypes::tar_cue_force(
        cache_is_outdated(
          gscache(),
          list(
            url = urlsym,
            sheet = sheet,
            range = range
          )
        )
      )
    ),
    targets::tar_target_raw(
      tarname,
      rlang::call2(
        "cachemeta_read_asset",
        as.name(tarname_file)
      )
    )
  )
}

#' Get a dataset from SurveyCTO
#'
#' Downloads a dataset from SurveyCTO, if the cached
#' copy is not stale. Otherwise, the cached copy will
#' be returned in the desired target.
#'
#' @param tarsym symbol - Used as the target name
#' @param id string - The ID of the particular form to download
#' @param auth list? (`NULL`) - Output of rsurveycto::scto_auth()
#' @param timeout integer? (3600L) - Rate limit timeout for the remote
#'        cache checking. If the last checked time was greater than
#'        `timeout` seconds ago, SurveyCTO will be queried for new
#'        new metadata.
#' @param password string? (`NULL`) - Password to use to (en|de)crypt the
#'        cached data
#' @param verbose boolean (`TRUE`) - Reports side-effect messages to stdout
#' @return list([[1]]: target, [[2]]: target)
tar_surveycto <- function(tarsym,
                          id,
                          auth = NULL,
                          timeout = 3600L,
                          password = NULL,
                          verbose = TRUE) {
  tarname <- as.character(substitute(tarsym))
  tarname_file <- paste0(tarname, "_file")

  sctocache_opts <- bquote(list(
    auth = .(substitute(auth)),
    timeout = .(timeout),
    verbose = .(verbose)
  ))

  sctocache_opts_unquoted <- list(
    auth = auth,
    timeout = timeout,
    verbose = verbose
  )

  getcall <- rlang::call2(
    "cache_get",
    bquote(surveycto_cache(passphrase = .(substitute(password)))),
    id,
    opts = sctocache_opts
  )

  list(
    targets::tar_target_raw(
      tarname_file,
      getcall,
      cue = tarchetypes::tar_cue_force(
        cache_is_outdated(
          surveycto_cache(passphrase = password),
          id,
          opts = sctocache_opts_unquoted
        )
      )
    ),
    targets::tar_target_raw(
      tarname,
      bquote(cachemeta_read_asset(.(as.name(tarname_file))))
    )
  )
}

tar_upload_nightly_reports <- function(targets,
                                       guid,
                                       target_patterns = c(
                                         "kfa_report",
                                         "psych_codebook"
                                       ),
                                       ...) {
  report_targets <- character()
  dots <- rlang::dots_list(...)

  for (pat in target_patterns) {
    tarnames <- tarchetypes::tar_select_names(targets, tidyselect::matches(pat))
    report_targets <- c(report_targets, tarnames)
  }

  if (length(report_targets) < 1) {
    return(targets)
  }

  report_syms <- lapply(report_targets, as.name)

  staging_target <- targets::tar_target_raw(
    "staging_nightly_reports",
    rlang::call2("c", !!!report_syms),
    cue = tarchetypes::tar_cue_force(TRUE)
  )

  upload_target <- targets::tar_target_raw(
    paste0("upload_nightly_reports_", guid),
    rlang::call2(
      "upload_nightly_reports",
      as.name(staging_target$settings$name),
      guid,
      upload = isTRUE(F_NIGHTLY),
      !!!dots
    ),
    cue = tarchetypes::tar_cue_force(isTRUE(F_NIGHTLY))
  )

  list(
    targets,
    staging_target,
    upload_target
  )
}

#' Load blueprints from current project
#'
#' Essentially a wrapper around `blueprintr::tar_blueprints`
#' that points to the blueprints within "blueprints/{TAR_PROJECT}"
#'
#' @param cur_proj Subfolder of "blueprints" -- defaults to `TAR_PROJECT`
#' @return blueprints for that project
tar_proj_blueprints <- function(cur_proj = TAR_PROJECT) {
  blueprintr::tar_blueprints(
    directory = here::here("inst/project/blueprints", cur_proj),
    recurse = TRUE
  )
}
