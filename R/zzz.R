.onLoad <- function(libname, pkgname) {
  op <- options()
  op_bpr <- list(
    blueprintr.interactive_eval_macros = FALSE,
    blueprintr.interactive_always_reload = TRUE,
    blueprintr.interactive_reload_warn = TRUE,
    blueprintr.attach_state = FALSE,
    blueprintr.use_local_metadata_path = FALSE
  )

  op_bpr[[improved_annotation_option()]] <- FALSE
  op_bpr[[variable_uuid_option()]] <- FALSE
  op_bpr[[empty_blueprints_dir_option()]] <- FALSE

  toset <- !(names(op_bpr) %in% names(op))
  if (any(toset)) options(op_bpr[toset])

  invisible()
}
