bpstep_create_metadata <- function(bp, ...) {
  bpstep(
    step = "create_metadata",
    bp = bp,
    payload = bpstep_payload(
      metadata_target_file_name(bp),
      metadata_call(bp),
      format = "file",
      ...
    )
  )
}

metadata_call <- function(bp) {
  deps <- blueprint_target_deps(bp)
  deps_syms <- lapply(deps, function(dep) as.name(paste0(dep, "_meta")))

  call2(
    "create_metadata_file",
    as.name(blueprint_target_name(bp)),
    as.name(blueprint_reference_name(bp)),
    !!!deps_syms,
    .ns = "blueprintr"
  )
}
