bpstep_build_initial <- function(asm, bp, ...) {
  assemble_bpstep(
    asm,
    step = "build_initial",
    bp = bp,
    payload = bpstep_payload(
      asm,
      target_name = blueprint_target_name(bp),
      target_command = translate_macros(bp$command)
    )
  )
}
