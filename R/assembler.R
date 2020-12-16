assembler <- function(executor, .class) {
  stopifnot(is.character(executor))

  # Subclass must be specified
  stopifnot(is.character(.class))

  structure(
    executor,
    class = c(.class, "bp_assembler")
  )
}

drake_assembler <- function() {
  if (!requireNamespace("drake", quietly = TRUE)) {
    bp_err("'drake' not installed.")
  }

  assembler("drake", "drake_assembler")
}

targets_assembler <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    bp_err("'targets' not installed.")
  }

  assembler("targets", "targets_assembler")
}
