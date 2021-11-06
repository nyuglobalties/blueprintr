bpstep_load_metadata <- function(asm, bp, ...) {
  assemble_bpstep(
    asm,
    step = "load_metadata",
    bp = bp,
    payload = bpstep_payload(
      asm,
      metadata_target_name(bp),
      metadata_load_call(bp),
      ...
    )
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
