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
                            verbose = FALSE,
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
      title = title,
      verbose = verbose
    ),
    quiet = TRUE,
    ...
  )
}
