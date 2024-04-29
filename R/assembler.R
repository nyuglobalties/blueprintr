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

assemble_bpstep <- function(asm, step) {
  bp_assert(is_bpstep(step))

  step$built_payload <- assemble_payload(asm, step$payload)

  structure(
    step,
    class = c("assembled_bpstep", class(step))
  )
}

assemble_payload <- function(asm, payload) {
  UseMethod("assemble_payload", asm)
}

#' @export
assemble_payload.drake_assembler <- function(asm, payload) {
  dots <- bpstep_payload_extra_args(payload)

  if (length(dots) > 0) {
    quoted_target <- rlang::call2(
      "target",
      command = payload$target_command,
      !!!dots,
      .ns = "drake"
    )
  } else {
    quoted_target <- payload$target_command
  }

  argslist <- list(quoted_target)
  names(argslist) <- payload$target_name

  do.call(drake::drake_plan, argslist)
}

#' @export
assemble_payload.targets_assembler <- function(asm, payload) {
  dots <- bpstep_payload_extra_args(payload)

  if ("pattern" %in% names(dots)) {
    dots[["pattern"]] <- bquote(quote(.(dots[["pattern"]])))
  }

  argslist <- rlang::list2(
    name = payload$target_name,
    command = bquote(quote(.(payload$target_command))),
    !!!dots
  )

  eval(rlang::call2(
    "tar_target_raw",
    !!!argslist,
    .ns = "targets"
  ))
}

assembly_steps <- function(asm, bp) {
  steps <- default_assembly_steps(bp)

  if (!is.null(bp$extra_steps)) {
    for (step in bp$extra_steps) {
      steps <- add_assembly_step(steps, step)
    }
  }

  if (isTRUE(bp$codebook_export)) {
    # The codebook export step is now defined as an extra step to faciliate
    # custom steps after final artifact is generated.
    lifecycle::deprecate_warn(
      "0.1.0",
      "blueprint(codebook_export)",
      "bp_export_codebook()"
    )

    steps[[length(steps) + 1]] <- bpstep_export_codebook(bp)
  }

  lapply(steps, function(x) assemble_bpstep(asm, x))
}

add_assembly_step <- function(steps, step) {
  bp_assert(is_bpstep(step), "`{substitute(step)}` must be a 'bpstep' object")
  bp_assert(all(vlapply(steps, is_bpstep)) || is.null(steps))

  if (!is.null(steps)) {
    step_names <- vcapply(steps, function(s) s$step)
    allow_duplicates <- step$allow_duplicates %||% FALSE

    if (step$step %in% step_names & !isTRUE(allow_duplicates)) {
      bp_err("'{step$step}' already found in blueprint '{step$blueprint$name}' steps") # nolint
    }

    steps[[length(steps) + 1]] <- step
  } else {
    steps <- list(step)
  }

  steps
}

default_assembly_steps <- function(bp) {
  if (file.exists(metadata_path(bp))) {
    meta_df <- load_metadata(bp)
  } else {
    meta_df <- NULL
  }

  list(
    bpstep_build_initial(bp),
    bpstep_blueprint_reference(bp),
    bpstep_create_metadata(bp),
    bpstep_load_metadata(bp),
    bpstep_check_data(bp, meta = meta_df),
    bpstep_cleanup(bp)
  )
}
