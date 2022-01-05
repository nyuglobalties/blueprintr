bpstep_cleanup <- function(bp, ...) {
  bpstep(
    step = "cleanup",
    bp = bp,
    payload = bpstep_payload(
      target_name = blueprint_final_name(bp),
      target_command = cleanup_call(bp),
      ...
    )
  )
}

cleanup_call <- function(bp) {
  bquote(blueprintr::cleanup(
    .(as.name(blueprint_checks_name(bp))),
    .(as.name(blueprint_target_name(bp))),
    .(as.name(blueprint_reference_name(bp))),
    .(as.name(metadata_target_name(bp)))
  ))
}
