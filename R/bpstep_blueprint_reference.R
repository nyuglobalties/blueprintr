bpstep_blueprint_reference <- function(assembler, bp, ...) {
  UseMethod("bpstep_blueprint_reference", assembler)
}

#' @export
bpstep_blueprint_reference.drake_assembler <- function(assembler, bp, ...) {
  arglist <- list(blueprint_ref_call(bp))
  names(arglist) <- blueprint_reference_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "blueprint_reference",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_blueprint_reference.targets_assembler <- function(assembler, bp, ...) {
  target <- targets::tar_target_raw(
    blueprint_reference_name(bp),
    blueprint_ref_call(bp)
  )

  targets_bpstep(
    step = "blueprint_reference",
    bp = bp,
    payload = target,
    ...
  )
}

blueprint_ref_call <- function(bp) {
  call2(
    "blueprint",
    !!!bp,
    .ns = "blueprintr"
  )
}
