bp_extend <- function(bp, ...) {
  bp_assert(inherits(bp, "blueprint"))

  dots <- rlang::dots_list(...)

  known_args <- names(formals(blueprint))
  known_args <- known_args[known_args != "..."]
  known_args <- known_args[known_args %in% names(bp)]

  unknown_params <- setdiff(names(bp), known_args)

  slice_known_args <- bp[known_args]
  slice_unknown_params <- bp[unknown_params]

  dots_known_args <- dots[names(dots) %in% known_args]
  dots_unknown_params <- dots[names(dots) %in% unknown_params]
  dots_remainder <- dots[setdiff(names(dots), c(known_args, unknown_params))]

  if (length(dots_known_args) > 0) {
    arg_select <- known_args[known_args %in% names(dots_known_args)]

    slice_known_args[arg_select] <- dots_known_args[arg_select]
  }

  if (length(dots_unknown_params) > 0) {
    arg_select <- unknown_params[unknown_params %in% names(dots_unknown_params)]

    slice_unknown_params[arg_select] <- dots_unknown_params[arg_select]
  }

  do.call(
    blueprint,
    rlang::list2(!!!slice_known_args, !!!slice_unknown_params, !!!dots_remainder)
  )
}

#' Instruct blueprint to export codebooks
#'
#' @param blueprint A blueprint
#' @param summaries Whether or not variable summaries should be included in codebook
#' @param file Path to where the codebook should be saved
#' @param template A path to an RMarkdown template
#' @param title Optional title of codebook
#' @param verbose If `TRUE`, logging messages during codebook render are displayed.
#'   Helpful during debugging.
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
  title = NULL,
  verbose = FALSE
) {
  bp_extend(
    blueprint,
    codebook_export = TRUE,
    codebook_summaries = summaries,
    codebook_file = file,
    codebook_template = template,
    codebook_title = title,
    codebook_verbose = verbose
  )
}

#' Convert variables to labelled variables in cleanup stage
#'
#' The [haven](https://haven.tidyverse.org/) package has a handy tool called
#' "labeled vectors", which are like factors that can be interpreted in other
#' statistical software like STATA and SPSS. See [haven::labelled()][haven::labelled]
#' for more information on the type. Running this on a blueprint will instruct
#' the blueprint to convert all variables with non-NA `title`, `description`, or
#' `coding` fields to labeled vectors.
#'
#' @param blueprint A blueprint
#' @return An amended blueprint with variable labelling in the cleanup phase set
#' @export
bp_label_variables <- function(blueprint) {
  bp_extend(
    blueprint,
    labelled = TRUE
  )
}

#' Include panelcleaner mapping on metadata creation
#' 
#' [panelcleaner](https://nyuglobalties.github.io/panelcleaner/]) defines
#' a mapping structure used for data import of panel, or more generally
#' longitudinal, surveys / data which can be used as a source for some
#' kinds of metadata (currently, only categorical coding information).
#' If the blueprint constructs a `mapped_df` object, then this extension
#' will signal to blueprintr to extract the mapping information and
#' include it.
#' 
#' @param blueprint A blueprint that may create a `mapped_df` data.frame
#' @return An amended blueprint with `mapped_df` metadata extraction set
#'         for metadata creation
#' @export 
bp_include_panelcleaner_meta <- function(blueprint) {
  bp_extend(
    blueprint,
    import_mdf_meta = TRUE
  )
}
