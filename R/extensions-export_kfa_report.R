#' Instruct blueprint to generate kfa report
#'
#' @param bp A blueprint
#' @param scale Which scale(s) to analyze
#' @param path Path(s) to where the report(s) should be saved
#' @param path_pattern Override the default location to save files
#'   (always rooted to the project root with here::here())
#' @param format The output format of the report(s)
#' @param title Optional title of report
#' @return An amended blueprint with the kfa report export instructions
#' @export
#' @examples
#' \dontrun{
#' test_bp <- blueprint(
#'   "mtcars_dat",
#'   description = "The mtcars dataset",
#'   command = mtcars
#' )
#'
#' new_bp <- test_bp %>% bp_export_codebook()
#' }
bp_export_kfa_report <- function(bp,
                                 scale,
                                 path = NULL,
                                 path_pattern = NULL,
                                 format = NULL,
                                 title = NULL) {
  bp_assert(is.character(scale))
  bp_assert(
    length(path) == length(scale) || is.null(path),
    "`path` must have the same length as `scale`"
  )
  bp_assert(
    length(title) == length(scale) || is.null(title),
    "`title` must have the same length as `scale`"
  )

  for (i in seq_along(scale)) {
    .s <- scale[i]
    .p <- if (is.null(path)) path[i] else NULL
    .t <- if (is.null(title)) title[i] else NULL

    bp <- bp_add_bpstep(
      bp,
      bpstep_export_kfa_report(
        bp,
        .s,
        path = .p,
        format = format,
        title = .t,
        path_pattern = path_pattern
      )
    )
  }

  bp
}

bpstep_export_kfa_report <- function(bp,
                                     scale,
                                     path = NULL,
                                     path_pattern = NULL,
                                     format = NULL,
                                     title = NULL) {
  snakecase_scale <- snakecase::to_snake_case(
    scale,
    transliterations = "ASCII-Latin"
  )
  target_name <- glue::glue("{bp$name}_{snakecase_scale}_kfa_report")

  bpstep(
    step = "export_kfa_report",
    bp = bp,
    payload = bpstep_payload(
      target_name = target_name,
      target_command = render_kfa_call(
        bp, scale, path,
        format, title,
        path_pattern = path_pattern
      )
    ),
    allow_duplicates = TRUE
  )
}

render_kfa_call <- function(bp,
                            scale,
                            path,
                            format,
                            title,
                            path_pattern = NULL) {
  out <- rlang::call2(
    "render_kfa_report",
    as.name(blueprint_final_name(bp)),
    as.name(blueprint_reference_name(bp)),
    as.name(metadata_target_name(bp)),
    scale = scale,
    path = path,
    format = format,
    title = title,
    .ns = "blueprintr"
  )

  if (!is.null(path_pattern)) {
    out[["path_pattern"]] <- path_pattern
  }

  out
}

#' Render k-fold factor analysis on scale using kfa
#'
#' Generates a k-fold factor analysis report using the 'scale'
#' field in the blueprintr data dictionaries. While not recommended,
#' this function does allow for multiple loaded variables, delimited by
#' commas. For example, 'var1' could have 'scale' be "SCALE1,SCALE2".
#'
#' @param dat Source data
#' @param bp The dataset's blueprint
#' @param meta blueprintr data dictionary
#' @param scale Scale identifier to be located in the 'scale' field
#' @param path Where to output the report; defaults to the "reports"
#'   subfolder of the current working *project* folder.
#' @param path_pattern If path is `NULL`, this is where the report will
#'   be saved. Variables available for use are:
#'   * `scale`: The scale name defined in the metadata
#'   * `snakecase_scale`: `scale` but in snake_case
#'   * `dat_name`: Name of the dataset (equivalent to the blueprint name)
#' @param format The output format; defaults to 'html_document'
#' @param title Optional title of the report
#' @return Path to where the generated report is saved
#' @export
render_kfa_report <- function(dat,
                              bp,
                              meta,
                              scale,
                              path = NULL,
                              path_pattern = "reports/kfa-{snakecase_scale}-{dat_name}.html", # nolint
                              format = NULL,
                              title = NULL) {
  dat_name <- blueprint_final_name(bp)
  snakecase_scale <- snakecase::to_snake_case(scale)

  path <- path %||% here::here(
    glue::glue(path_pattern)
  )

  format <- format %||% "html_document"
  title <- title %||% glue::glue("'{scale}' KFA Report")

  if (!requireNamespace("kfa", quietly = TRUE)) {
    bp_err("Cannot render kfa reports. Please install kfa.")
  }

  if (!"scale" %in% names(meta)) {
    bp_err(c(
      "Cannot render kfa report for '{scale}': ",
      "'scale' not found in `{substitute(meta)}`"
    ))
  }

  meta <- dplyr::mutate(
    meta,
    .scale = strsplit(.data$scale, "\\,\\s*")
  )

  scale_vars <- meta[
    vlapply(meta$.scale, function(.s) scale %in% .s),
    "name",
    drop = TRUE
  ]

  if (length(scale_vars) == 0) {
    bp_err(c(
      "Cannot render kfa report for '{scale}': ",
      "No variables are linked to this scale identifier"
    ))
  }

  scale_dat <- dplyr::select(
    dat,
    tidyselect::all_of(scale_vars)
  )

  models <- kfa::kfa(variables = scale_dat)
  report <- kfa::kfa_report(
    models,
    file.name = path,
    report.format = format,
    report.title = title
  )

  path
}