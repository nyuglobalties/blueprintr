bpstep_check_data <- function(assembler, bp, meta, ...) {
  UseMethod("bpstep_check_data", assembler)
}

#' @export
bpstep_check_data.drake_assembler <- function(assembler, bp, meta = NULL, ...) {
  arglist <- list(check_data_call(bp, meta))
  names(arglist) <- blueprint_checks_name(bp)

  plan <- do.call(drake::drake_plan, arglist)

  drake_bpstep(
    step = "check_data",
    bp = bp,
    payload = plan,
    ...
  )
}

#' @export
bpstep_check_data.targets_assembler <- function(assembler, bp, meta = NULL, ...) {
  target <- targets::tar_target_raw(
    blueprint_checks_name(bp),
    check_data_call(bp, meta)
  )

  targets_bpstep(
    step = "check_data",
    bp = bp,
    payload = target,
    ...
  )
}

check_data_call <- function(bp, meta) {
  default_checks <- list(
    bquote(all_variables_present(.META(.(bp$name)), .BLUEPRINT(.(bp$name)))),
    bquote(all_types_match(.META(.(bp$name))))
  )

  if (!is.null(bp$base_checks)) {
    bp_assert(
      inherits(bp$base_checks, "check_list"),
      "bp checks must be a 'check_list'"
    )

    default_checks <- bp$base_checks
  }

  if (!is.null(bp$checks)) {
    bp_assert(
      inherits(bp$checks, "check_list"),
      "bp checks must be a 'check_list'"
    )

    content_checks <- bp$checks
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
    blueprint_target_name(bp)
  )

  if (!is.null(meta) && ".parsed_tests" %in% names(meta)) {
    variable_checks <-  purrr::map2(
      meta$.parsed_tests,
      meta$name,
      function(.t, .n) {
        lapply(.t, interpret_raw_check, blueprint_target_name(bp), variable = .n)
      }
    )
    variable_checks <- purrr::flatten(variable_checks)
  } else {
    variable_checks <- list()
  }

  all_checks <- c(all_checks, variable_checks)

  call2(
    "eval_checks",
    !!!all_checks,
    .ns = "blueprintr"
  )
}
