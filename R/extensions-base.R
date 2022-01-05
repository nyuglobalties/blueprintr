#' Add custom elements to a blueprint
#'
#' `blueprint()` objects are essentially just `list()` objects
#' that contain a bunch of metadata on the data asset construction.
#' Use `bp_extend()` to set or add new elements.
#'
#' @param bp A blueprint
#' @param ... Keyword arguments forwarded to blueprint()
#'
#' @export
#' @examples
#' if (FALSE) {
#'   bp <- blueprint("some_blueprint", ...)
#'   adjusted_bp <- bp_extend(bp, new_option = TRUE)
#'   bp_with_annotation_set <- bp_extend(bp, annotate = TRUE)
#' }
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
    rlang::list2(
      !!!slice_known_args,
      !!!slice_unknown_params,
      !!!dots_remainder
    )
  )
}

#' Add custom bpstep to blueprint schema
#'
#' `blueprint()` objects store custom [bpstep][bpstep] objects
#' in the "extra_steps" element. This function adds a new
#' step to that element.
#'
#' @param bp A blueprint
#' @param step A bpstep object
#'
#' @export
#' @examples
#' if (FALSE) {
#'   # Based on the codebook export step
#'   step <- bpstep(
#'     step = "export_codebook",
#'     bp = bp,
#'     payload = bpstep_payload(
#'       target_name = blueprint_codebook_name(bp),
#'       target_command = codebook_export_call(bp),
#'       format = "file",
#'       ...
#'     )
#'   )
#'
#'   bp_add_bpstep(
#'     bp,
#'     step
#'   )
#' }
bp_add_bpstep <- function(bp, step) {
  bp_assert(is_blueprint(bp))
  bp_assert(is_bpstep(step))

  steps <- add_assembly_step(bp$extra_steps, step)
  bp$extra_steps <- steps
  bp
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