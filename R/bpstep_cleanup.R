bpstep_cleanup <- function(assembler, bp, ...) {
  UseMethod("bpstep_cleanup", assembler)
}

#' @export
bpstep_cleanup.drake_assembler <- function(assembler, bp, ...) {
  arglist <- list(cleanup_call(bp))
  names(arglist) <- blueprint_final_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "cleanup",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_cleanup.targets_assembler <- function(assembler, bp, ...) {
  target <- targets::tar_target_raw(
    blueprint_final_name(bp),
    cleanup_call(bp)
  )

  targets_bpstep(
    step = "cleanup",
    bp = bp,
    payload = target,
    ...
  )
}

cleanup_call <- function(bp) {
  bquote(blueprintr::accept_content(
    .(as.name(blueprint_checks_name(bp))),
    .(as.name(blueprint_target_name(bp))),
    .(as.name(blueprint_reference_name(bp))),
    .(as.name(metadata_target_name(bp)))
  ))
}
