#' @export
plan_from_blueprint <- function(blueprint) {
  plan <- drake::drake_plan()

  # drake_plan() does not add the class by default
  plan <- structure(
    plan,
    class = c("drake_plan", class(plan))
  )

  attach_blueprint(plan, blueprint)
}

#' @export
attach_blueprints <- function(plan, ...) {
  dots <- dots_list(...)

  for (blueprint in dots) {
    plan <- attach_blueprint(plan, blueprint)
  }

  plan
}

#' @export
attach_blueprint <- function(plan, blueprint) {
  stopifnot(inherits(plan, "drake_plan"))
  stopifnot(inherits(blueprint, "blueprint"))

  plan <- add_blueprint_target(plan, blueprint)
  plan <- add_blueprint_reference(plan, blueprint)

  if (!metadata_file_exists(blueprint)) {
    meta <- NULL
    plan <- add_metadata_creation_target(plan, blueprint)
  } else {
    meta <- load_metadata(blueprint)
    plan <- add_blueprint_metadata(plan, blueprint, meta)
  }

  add_content_checks(plan, blueprint, meta = meta)
}

add_blueprint_target <- function(plan, blueprint) {
  command_ast <- extract_ast(blueprint$command)
  command_ast <- modify_ast_if(command_ast, is_target_ast, eval_ast)
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
  deps <- blueprint_deps(blueprint)
  deps_syms <- lapply(deps, function(dep) as.name(paste0(dep, "_meta")))

  command <- call2(
    "create_metadata_file",
    as.name(blueprint_target_name(blueprint)),
    as.name(blueprint_reference_name(blueprint)),
    !!!deps_syms
  )

  arglist <- list2(
    !!metadata_target_name(blueprint) := command
  )

  target_plan <- do.call(drake::drake_plan, arglist)
  drake::bind_plans(plan, target_plan)
}

add_content_checks <- function(plan, blueprint, meta = NULL) {
  default_checks <- list(
    bquote(all_variables_present(.META(.(blueprint$name)), .BLUEPRINT(.(blueprint$name)))),
    bquote(all_types_match(.META(.(blueprint$name))))
  )

  if (!is.null(blueprint$base_checks)) {
    bp_assert(
      inherits(blueprint$base_checks, "check_list"),
      "Blueprint checks must be a 'check_list'"
    )

    default_checks <- blueprint$base_checks
  }

  if (!is.null(blueprint$checks)) {
    bp_assert(
      inherits(blueprint$checks, "check_list"),
      "Blueprint checks must be a 'check_list'"
    )

    content_checks <- blueprint$checks
  } else {
    content_checks <- list()
  }

  all_checks <- rlang::list2(
    !!!default_checks,
    !!!content_checks
  )

  all_checks <- lapply(
    all_checks,
    interpret_raw_check,
    blueprint_target_name(blueprint)
  )

  if (!is.null(meta) && !is.null(meta$tests)) {
    variable_checks <-  purrr::map2(
      meta$tests,
      meta$name,
      function(.t, .n) {
        lapply(.t, interpret_raw_check, blueprint_target_name(blueprint), variable = .n)
      }
    )
    variable_checks <- purrr::flatten(variable_checks)
  } else {
    variable_checks <- list()
  }

  all_checks <- c(all_checks, variable_checks)

  command1 <- rlang::call2(
    "eval_checks",
    !!!all_checks
  )

  arglist <- list2(
    !!blueprint_checks_name(blueprint) := command1
  )

  target_plan <- do.call(drake::drake_plan, arglist)
  plan <- drake::bind_plans(plan, target_plan)

  command2 <- bquote(accept_content(
    .(as.name(blueprint_checks_name(blueprint))),
    .(as.name(blueprint_target_name(blueprint))),
    .(as.name(blueprint_reference_name(blueprint))),
    .(as.name(metadata_target_name(blueprint)))
  ))

  arglist <- list2(
    !!blueprint_final_name(blueprint) := command2
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
