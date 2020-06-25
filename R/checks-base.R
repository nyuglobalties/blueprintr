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

#' Evaluate all checks on a blueprint
#'
#' Runs all checks -- dataset and variable -- on a blueprint
#' to determine if a built dataset passes all restrictions.
#'
#' @param ... All quoted check calls
#' @param .env The environment in which the calls are evaluated
#'
#' @export
eval_checks <- function(..., .env = parent.frame()) {
  checks_dt <- checks(...)

  checks_dt$.pass <- vlapply(checks_dt$check_func, function(f) eval(f, envir = .env))

  if (any(checks_dt$.pass == FALSE)) {
    checks_error(checks_dt)
  }

  checks_dt
}

checks_error <- function(checks) {
  false_funcs <-
    checks %>% 
    dplyr::filter(.data$.pass == FALSE) %>% 
    dplyr::pull(.data$check_func) %>% 
    vcapply(safe_deparse, collapse = " ", trim = TRUE)

  err_msgs <- glue("`{false_funcs}` is not TRUE")

  rlang::abort(
    glue_collapse(err_msgs, "\n"),
    checks = checks,
    .subclass = "checks_error"
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
