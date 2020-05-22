plan_from_blueprint <- function(blueprint) {
  plan <- drake::drake_plan()

  # drake_plan() does not add the class by default
  plan <- structure(
    plan,
    class = c("drake_plan", class(plan))
  )

  attach_blueprint(plan, blueprint)
}

attach_blueprints <- function(plan, ...) {
  dots <- dots_list(...)

  for (blueprint in dots) {
    plan <- attach_blueprint(plan, blueprint)
  }

  plan
}

attach_blueprint <- function(plan, blueprint) {
  stopifnot(inherits(plan, "drake_plan"))
  stopifnot(inherits(blueprint, "blueprint"))

  plan <- add_blueprint_target(plan, blueprint)
  plan <- add_blueprint_reference(plan, blueprint)

  if (!metadata_file_exists(blueprint)) {
    plan <- add_metadata_creation_target(plan, blueprint)
  } else {
    meta <- load_metadata(blueprint)

    plan <- add_blueprint_metadata(plan, blueprint, meta)
    plan <- add_content_checks(plan, blueprint, meta)
  }

  plan
}

add_blueprint_target <- function(plan, blueprint) {
  command_ast <- extract_ast(blueprint$command)
  command_ast <- modify_ast_if(command_ast, is_chunk_ast, eval_chunk)
  translated_command <- collapse_ast(command_ast)

  arglist <- list2(!!blueprint_target_name(blueprint) := translated_command)
  target_plan <- do.call(drake::drake_plan, arglist)

  drake::bind_plans(plan, target_plan)
}

add_blueprint_reference <- function(plan, blueprint) {
  command <- call2("blueprint", !!!blueprint)

  arglist <- list2(!!blueprint_reference_name(blueprint) := command)
  target_plan <- do.call(drake::drake_plan, arglist)

  drake::bind_plans(plan, target_plan)
}

add_blueprint_metadata <- function(plan, blueprint, meta) {
  stopifnot(is.data.frame(meta))

  meta <- as.data.frame(meta)
  command <- call2("data.frame", !!!meta, stringsAsFactors = FALSE)
  command <- call2("metadata", command)

  arglist <- list2(!!metadata_target_name(blueprint) := command)
  target_plan <- do.call(drake::drake_plan, arglist)

  drake::bind_plans(plan, target_plan)
}

add_metadata_creation_target <- function(plan, blueprint) {
  command <- bquote(create_metadata_file(
    .(as.name(blueprint_target_name(blueprint))),
    .(as.name(blueprint_reference_name(blueprint)))
  ))

  arglist <- list2(
    !!paste0(blueprint_target_name(blueprint), "_metadata_export") := command
  )

  target_plan <- do.call(drake::drake_plan, arglist)
  drake::bind_plans(plan, target_plan)
}

add_content_checks <- function(plan, blueprint, meta) {
  command1 <- bquote(check_content(
    .(as.name(blueprint_target_name(blueprint))),
    .(as.name(blueprint_reference_name(blueprint))),
    .(as.name(metadata_target_name(blueprint)))
  ))

  arglist <- list2(
    !!paste0(blueprint_target_name(blueprint), "_content_checks") := command1
  )

  target_plan <- do.call(drake::drake_plan, arglist)
  plan <- drake::bind_plans(plan, target_plan)

  command2 <- bquote(accept_content(
    .(as.name(paste0(blueprint_target_name(blueprint), "_content_checks"))),
    .(as.name(blueprint_reference_name(blueprint)))
  ))

  arglist <- list2(
    !!paste0(blueprint_target_name(blueprint), "_content_pass") := command2
  )

  target_plan <- do.call(drake::drake_plan, arglist)
  drake::bind_plans(plan, target_plan)
}

deparse_lang_cols <- function(plan) {
  for (col in lang_cols(plan)) {
    plan[[col]] <- deparse_lang_col(plan[[col]])
  }
  plan
}

deparse_lang_col <- function(x) {
  if (!length(x) || !is.list(x)) {
    return(x)
  }

  out <- unlist(lapply(x, safe_deparse, collapse = " ", backtick = TRUE))
  structure(out, class = "expr_list")
}

lang_cols <- function(plan) {
  intersect(colnames(plan), c("command", "dynamic", "trigger", "transform"))
}
