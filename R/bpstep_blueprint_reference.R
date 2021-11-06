bpstep_blueprint_reference <- function(asm, bp, ...) {
  assemble_bpstep(
    asm,
    step = "blueprint_reference",
    bp = bp,
    payload = bpstep_payload(
      asm,
      target_name = blueprint_reference_name(bp),
      target_command = blueprint_ref_call(bp),
      ...
    )
  )
}

blueprint_ref_call <- function(bp) {
  call2(
    "blueprint",
    !!!bp,
    .ns = "blueprintr"
  )
}
