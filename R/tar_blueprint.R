#' @export
tar_blueprint <- function(...) {
  asm <- targets_assembler()
  bp <- blueprint(...)

  if (file.exists(metadata_path(bp))) {
    meta_df <- load_metadata(bp)
  } else {
    meta_df <- NULL
  }

  steps <- list(
    bpstep_build_initial(asm, bp),
    bpstep_blueprint_reference(asm, bp),
    bpstep_create_metadata(asm, bp),
    bpstep_load_metadata(asm, bp),
    bpstep_check_data(asm, bp, meta = meta_df),
    bpstep_cleanup(asm, bp)
  )

  if (isTRUE(bp$codebook_export)) {
    steps[[length(steps) + 1]] <- bpstep_export_codebook(asm, bp)
  }

  lapply(steps, function(step) step$payload)
}
