bpstep_create_metadata <- function(assembler, bp, ...) {
  UseMethod("bpstep_create_metadata", assembler)
}

#' @export
bpstep_create_metadata.drake_assembler <- function(assembler, bp, ...) {
  arglist <- list(
    bquote(target(
      command = .(metadata_call(bp)),
      format = "file"
    ))
  )
  names(arglist) <- metadata_target_file_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "create_metadata",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_create_metadata.targets_assembler <- function(assembler, bp, ...) {
  target <- targets::tar_target_raw(
    metadata_target_file_name(bp),
    metadata_call(bp),
    format = "file"
  )

  targets_bpstep(
    step = "create_metadata",
    bp = bp,
    payload = target,
    ...
  )
}

metadata_call <- function(bp) {
  deps <- blueprint_deps(bp)
  deps_syms <- lapply(deps, function(dep) as.name(paste0(dep, "_meta")))

  call2(
    "create_metadata_file",
    as.name(blueprint_target_name(bp)),
    as.name(blueprint_reference_name(bp)),
    !!!deps_syms,
    .ns = "blueprintr"
  )
}
