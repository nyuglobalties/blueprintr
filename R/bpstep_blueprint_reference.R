bpstep_blueprint_reference <- function(bp, ...) {
  bpstep(
    step = "blueprint_reference",
    bp = bp,
    payload = bpstep_payload(
      target_name = blueprint_reference_name(bp),
      target_command = blueprint_ref_call(bp),
      ...
    )
  )
}

blueprint_ref_call <- function(bp) {
  rlang::call2(
    "blueprint",
    !!!bp,
    .ns = "blueprintr"
  )
}
