bpstep_export_codebook <- function(bp, ...) {
  bpstep(
    step = "export_codebook",
    bp = bp,
    payload = bpstep_payload(
      target_name = blueprint_codebook_name(bp),
      target_command = codebook_export_call(bp),
      format = "file",
      ...
    )
  )
}

codebook_export_call <- function(bp) {
  default_codebook_file <- here::here("codebooks", paste0(bp$name, ".html"))
  codebook_file <- bp$codebook_file %||% default_codebook_file
  codebook_template <- bp$codebook_template %||% NULL
  codebook_title <- bp$codebook_title %||% NULL
  with_data <- bp$codebook_summaries %||% FALSE

  command <- call2(
    "render_codebook",
    as.name(blueprint_reference_name(bp)),
    as.name(metadata_target_name(bp)),
    codebook_file,
    dataset = if (with_data) as.name(blueprint_final_name(bp)) else NULL,
    .ns = "blueprintr"
  )

  if (!is.null(codebook_template)) {
    command[["template"]] <- bquote(knitr_in(.(codebook_template)))
  }

  if (!is.null(codebook_title)) {
    command[["title"]] <- codebook_title
  }

  call2("{", command, codebook_file)
}
