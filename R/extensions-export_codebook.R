#' Instruct blueprint to export codebooks
#'
#' @param blueprint A blueprint
#' @param summaries Whether or not variable summaries should be included in codebook
#' @param file Path to where the codebook should be saved
#' @param template A path to an RMarkdown template
#' @param title Optional title of codebook
#' @return An amended blueprint with the codebook export instructions
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
bp_export_codebook <- function(
  blueprint,
  summaries = FALSE,
  file = NULL,
  template = NULL,
  title = NULL
) {
  bp <- bp_extend(
    blueprint,
    codebook_summaries = summaries,
    codebook_file = file,
    codebook_template = template,
    codebook_title = title
  )

  bp_add_bpstep(
    bp,
    bpstep_export_codebook(bp)
  )
}

bpstep_export_codebook <- function(bp, ...) {
  bpstep(
    step = "export_codebook",
    bp = bp,
    payload = bpstep_payload(
      target_name = blueprint_codebook_name(bp),
      target_command = codebook_export_call(bp),
      format = "file",
      ...
    )
  )
}

codebook_export_call <- function(bp) {
  default_codebook_file <- here::here("codebooks", paste0(bp$name, ".html"))
  codebook_file <- bp$codebook_file %||% default_codebook_file
  codebook_template <- bp$codebook_template %||% NULL
  codebook_title <- bp$codebook_title %||% NULL
  with_data <- bp$codebook_summaries %||% FALSE

  command <- call2(
    "render_codebook",
    as.name(blueprint_reference_name(bp)),
    as.name(metadata_target_name(bp)),
    codebook_file,
    dataset = if (with_data) as.name(blueprint_final_name(bp)) else NULL,
    .ns = "blueprintr"
  )

  if (!is.null(codebook_template)) {
    command[["template"]] <- bquote(knitr_in(.(codebook_template)))
  }

  if (!is.null(codebook_title)) {
    command[["title"]] <- codebook_title
  }

  call2("{", command, codebook_file)
}

#' Render codebooks for datasets
#'
#' @param blueprint A dataset blueprint
#' @param meta A `blueprint_metadata` object related to the blueprint
#' @param file Path to where the codebook should be saved
#' @param title Title of the codebook
#' @param dataset If included, a `data.frame` to be used as a source for
#'                summaries
#' @param template Path to the knitr template
#' @param ... Extra parameters passed to [rmarkdown::render()][rmarkdown::render()]
#'
#' @export
#'
render_codebook <- function(blueprint,
                            meta,
                            file,
                            title = glue("{ui_value(blueprint$name)} Codebook"),
                            dataset = NULL,
                            template = bp_path("codebook_templates/default_codebook.Rmd"),
                            ...) {
  bp_assert(inherits(blueprint, "blueprint"))
  bp_assert(inherits(meta, "blueprint_metadata"))
  bp_assert(is.data.frame(dataset) || is.null(dataset))

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    bp_err("rmarkdown must be installed to render codebooks")
  }

  if (!requireNamespace("rcoder", quietly = TRUE)) {
    bp_err("rcoder must be installed to render codebooks")
  }

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    bp_err("kableExtra must be installed to render codebooks")
  }

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
    message(glue("Created '{dirname(file)}'"))
  }

  rmarkdown::render(
    template,
    output_file = file,
    params = list(
      blueprint = blueprint,
      meta = meta,
      dataset = dataset,
      title = title
    ),
    quiet = TRUE,
    ...
  )
}