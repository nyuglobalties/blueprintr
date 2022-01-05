bpstep_build_initial <- function(bp, ...) {
  bpstep(
    step = "build_initial",
    bp = bp,
    payload = bpstep_payload(
      target_name = blueprint_target_name(bp),
      target_command = translate_macros(bp$command)
    )
  )
}
