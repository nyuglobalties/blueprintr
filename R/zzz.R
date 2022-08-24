.onLoad <- function(libname, pkgname) {
  op <- options()
  op_bpr <- list(
    blueprintr.interactive_eval_macros = FALSE,
    blueprintr.interactive_reload_warn = TRUE,
    blueprintr.attach_state = FALSE
  )

  toset <- !(names(op_bpr) %in% names(op))
  if (any(toset)) options(op_bpr[toset])

  invisible()
}
