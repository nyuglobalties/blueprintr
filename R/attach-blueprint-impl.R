#' @export
attach_blueprint.blueprint <- function(plan, blueprint) {
  stopifnot(inherits(plan, "drake_plan"))

  plan <- add_blueprint_target(plan, blueprint)
  plan <- add_blueprint_reference(plan, blueprint)

  if (!metadata_file_exists(blueprint)) {
    meta <- NULL
    plan <- add_metadata_creation_target(plan, blueprint)
  } else {
    meta <- load_metadata(blueprint)
    plan <- add_blueprint_metadata(plan, blueprint, meta)
  }

  if (isTRUE(blueprint$codebook_export)) {
    plan <- add_codebook_export(plan, blueprint)
  }

  add_content_checks(plan, blueprint, meta = meta)
}

add_draft_target <- function(plan, blueprint) {
  translated_command <- translate_command(blueprint)

  arglist <- list2(!!blueprint_draft_name(blueprint) := translated_command)
  target_plan <- do.call(drake::drake_plan, arglist)

  drake::bind_plans(plan, target_plan)
}

add_info_target <- function(plan, blueprint) {
  draft_symbol <- as.name(blueprint_draft_name(blueprint))
  meta_file <- metadata_path(blueprint)
  deps_syms <- lapply(blueprint_deps(blueprint), as.name)

  metadata_creation_call <-

  info_snippet <- bquote({
    bp <- .(blueprint)
    meta_location <- .(meta_file)

    if (!file.exists(meta_location)) {
      meta <- create_metadata_file(
        .(draft_symbol),
        bp,
        !!!lapply()
      )
    }
  })
}

metadata_creation_call <- function(blueprint) {
  deps_syms <- lapply(blueprint_deps(blueprint), as.name)

  call2(
    "create_metadata_file",
    as.name(blueprint_draft_name(blueprint)),
    quote(bp),
    !!!deps_syms
  )
}
