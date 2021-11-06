assembler <- function(executor, .class) {
  stopifnot(is.character(executor))

  # Subclass must be specified
  stopifnot(is.character(.class))

  structure(
    executor,
    class = c(.class, "bp_assembler", class(executor))
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

assembly_steps <- function(asm, bp) {
  steps <- default_assembly_steps(asm, bp)

  if (!is.null(bp$extra_steps)) {
    for (step in bp$extra_steps) {
      steps <- add_assembly_step(steps, step)
    }
  }

  if (isTRUE(bp$codebook_export)) {
    steps[[length(steps) + 1]] <- bpstep_export_codebook(asm, bp)
  }

  steps
}

add_assembly_step <- function(steps, step) {
  bp_assert(is_bpstep(step), "`{substitute(step)}` must be a 'bpstep' object")
  bp_assert(all(vlapply(steps, is_bpstep)) || is.null(steps))

  if (!is.null(steps)) {
    step_names <- vcapply(steps, function(s) s$step)

    if (step$step %in% step_names) {
      bp_err("'{step$step}' already found in blueprint '{bp$name}' steps")
    }

    steps[[length(steps) + 1]] <- step
  } else {
    steps <- list(step)
  }

  steps
}

default_assembly_steps <- function(asm, bp) {
  if (file.exists(metadata_path(bp))) {
    meta_df <- load_metadata(bp)
  } else {
    meta_df <- NULL
  }

  list(
    bpstep_build_initial(asm, bp),
    bpstep_blueprint_reference(asm, bp),
    bpstep_create_metadata(asm, bp),
    bpstep_load_metadata(asm, bp),
    bpstep_check_data(asm, bp, meta = meta_df),
    bpstep_cleanup(asm, bp)
  )
}
