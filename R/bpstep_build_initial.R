bpstep_build_initial <- function(assembler, bp, ...) {
  UseMethod("bpstep_build_initial", assembler)
}

#' @export
bpstep_build_initial.drake_assembler <- function(assembler, bp, ...) {
  arglist <- list(translate_macros(bp$command))
  names(arglist) <- blueprint_target_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "build_initial",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_build_initial.targets_assembler <- function(assembler, bp, ...) {
  target <- targets::tar_target_raw(
    blueprint_target_name(bp),
    translate_macros(bp$command)
  )

  targets_bpstep(
    step = "build_initial",
    bp = bp,
    payload = target,
    ...
  )
}
