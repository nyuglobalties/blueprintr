checks <- function(...) {
  dots <- as.list(substitute(list(...))[-1])

  checklist <- lapply(dots, check)
  check_dat <- dplyr::tibble(
    check_func = lapply(checklist, function(x) x$check_func),
    target = vcapply(checklist, function(x) x$target),
    variable = vcapply(checklist, function(x) if (is.null(x$variable)) NA_character_ else x$variable)
  )

  structure(
    check_dat,
    class = c("checks", class(check_dat))
  )
}

check <- function(func) {
  stopifnot(rlang::is_call(func))

  structure(
    list(
      check_func = func,
      target = check_func_target(func),
      variable = check_func_variable(func)
    ),
    class = "check"
  )
}

check_func_target <- function(func) {
  first_arg <- rlang::call_args(func)[[1]]

  if (is_variable_check_func(func)) {
    as.character(node_cadr(first_arg))
  } else {
    as.character(first_arg)
  }
}

check_func_variable <- function(func) {
  if (is_variable_check_func(func)) {
    first_arg <- rlang::call_args(func)[[1]]

    as.character(node_cddr(first_arg)[[1]])
  } else {
    NULL
  }
}

is_variable_check_func <- function(func) {
  first_arg <- rlang::call_args(func)[[1]]
  arg_ast <- extract_ast(first_arg)

  if (!is_ast(arg_ast)) {
    return(FALSE)
  }

  identical(arg_ast$head, "$")
}

check_errors_attr <- "check.errors"

has_check_errors <- function(x) {
  has_attr(x, check_errors_attr)
}

get_check_errors <- function(x) {
  get_attr(x, check_errors_attr)
}

set_check_errors <- function(x, reasons) {
  set_attr(x, check_errors_attr, reasons)
}

#' Helper function to embed reasons
#' @noRd
fail_check <- function(reasons = NULL) {
  set_check_errors(FALSE, reasons)
}

#' Helper function to embed reasons
#' @noRd
warn_check <- function(reasons = NULL) {
  set_check_errors(TRUE, reasons)
}

#' Evaluate all checks on a blueprint
#'
#' Runs all checks -- dataset and variable -- on a blueprint
#' to determine if a built dataset passes all restrictions.
#'
#' @details # Check functions
#'
#' Check functions are simple functions that take in either
#' a data.frame or variable at the minimum, plus some extra
#' arguments if need, and returns a logical value: `TRUE` or `FALSE.`
#' In blueprintr, the entire check passes or fails unlike other
#' testing frameworks like {pointblank}. If you'd like to embed
#' extra context for your test result, modify the "check.errors"
#' attribute of the returned logical value with a character vector
#' which will be rendered into a bulleted list. Note: if you embed
#' reasons for a `TRUE`, the check will produce a warning in the drake
#' or targets pipeline.
#'
#' @param ... All quoted check calls
#' @param .env The environment in which the calls are evaluated
#'
#' @export
eval_checks <- function(..., .env = parent.frame()) {
  checks_dt <- checks(...)

  checks_results <- lapply(checks_dt$check_func, function(f) eval(f, envir = .env))
  checks_dt$.pass <- vlapply(checks_results, as.logical)
  checks_dt$.fail_meta <- vcapply(checks_results, \(x) {
    if (has_check_errors(x)) {
      errs <- get_check_errors(x)
      errs <- vcapply(errs, \(x) paste0("  * ", x))
      paste0(":\n", paste0(errs, collapse = "\n"))
    } else {
      ""
    }
  })

  if (any(checks_dt$.pass == TRUE & checks_dt$.fail_meta != "")) {
    checks_warn(checks_dt)
  }

  if (any(checks_dt$.pass == FALSE)) {
    checks_error(checks_dt)
  }

  checks_dt
}

checks_error <- function(checks) {
  checks <- checks %>%
    dplyr::mutate(
      .call = vcapply(.data$check_func, safe_deparse, collapse = " ", trim = TRUE),
    ) %>%
    dplyr::mutate(
      .message = dplyr::if_else(
        .data$.pass == FALSE,
        paste0(
          "`", .data$.call, "` is not TRUE", .data$.fail_meta
        ),
        NA_character_
      ),
      # Include the warning here for user experience
      .message = dplyr::if_else(
        .data$.pass == TRUE & .data$.fail_meta != "",
        paste0(
          "`", .data$.call, "` has some potential issues", .data$.fail_meta
        ),
        .data$.message
      )
    )

  err_msgs <- checks %>%
    dplyr::filter(!is.na(.data$.message)) %>%
    dplyr::pull(.data$.message)

  rlang::abort(
    glue_collapse(err_msgs, "\n"),
    class = "checks_error",
    checks = checks
  )
}

checks_warn <- function(checks) {
  checks <- checks %>%
    dplyr::mutate(
      .call = vcapply(.data$check_func, safe_deparse, collapse = " ", trim = TRUE),
    ) %>%
    dplyr::mutate(
      .message = dplyr::if_else(
        .data$.pass == TRUE & .data$.fail_meta != "",
        paste0(
          "`", .data$.call, "` has some potential issues", .data$.fail_meta
        ),
        NA_character_
      )
    )

  warn_msgs <- checks %>%
    dplyr::filter(!is.na(.data$.message)) %>%
    dplyr::pull(.data$.message)

  rlang::warn(
    message = glue_collapse(warn_msgs, "\n"),
    class = "checks_warn",
    checks = checks
  )
}

#' Create a quoted list of check calls
#'
#' @param ... A collection of calls to be used for checks
#' @export
check_list <- function(...) {
  if (missing(...)) {
    return(structure(list(), class = "check_list"))
  }

  dots <- as.list(substitute(list(...))[-1])

  structure(
    lapply(dots, clean_check_command),
    class = "check_list"
  )
}

#' @export
print.check_list <- function(x, ...) {
  cat_line("<check list>")
  stripped <- c(x)

  for (check in stripped) {
    print(check)
  }

  invisible(NULL)
}

clean_check_command <- function(check) {
  if (rlang::is_symbol(check)) {
    check <- rlang::call2(check)
  }

  check
}

interpret_raw_check <- function(func, target, variable = NULL) {
  stopifnot(rlang::is_call(func))
  stopifnot(is.character(target))
  stopifnot(is.character(variable) || is.null(variable))

  call_head <- list(
    name = rlang::call_name(func),
    ns = rlang::call_ns(func)
  )

  if (!is.null(variable)) {
    first_arg <- bquote(`$`(.(as.name(target)), .(as.name(variable))))
  } else {
    first_arg <- as.name(target)
  }

  new_func <- rlang::call2(
    call_head$name,
    first_arg,
    !!!rlang::call_args(func),
    .ns = call_head$ns
  )

  is_any_macro <- function(ast) {
    is_macro_ast(ast, c(".TARGET", ".BLUEPRINT", ".META"))
  }

  new_func_ast <- extract_ast(new_func)
  new_func_ast <- modify_ast_if(new_func_ast, is_any_macro, eval_ast)
  collapse_ast(new_func_ast)
}
