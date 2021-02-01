bpstep_load_metadata <- function(assembler, bp, ...) {
  UseMethod("bpstep_load_metadata", assembler)
}

#' @export
bpstep_load_metadata.drake_assembler <- function(assembler, bp, ...) {
  arglist <- list(metadata_load_call(bp))
  names(arglist) <- metadata_target_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "load_metadata",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_load_metadata.targets_assembler <- function(assembler, bp, ...) {
  target <- targets::tar_target_raw(
    metadata_target_name(bp),
    metadata_load_call(bp)
  )

  targets_bpstep(
    step = "load_metadata",
    bp = bp,
    payload = target,
    ...
  )
}

metadata_load_call <- function(bp) {
  # TODO: Support more than CSV docs
  bquote(
    blueprintr::read_metadata(
      .(as.name(metadata_target_file_name(bp)))
    )
  )
}
