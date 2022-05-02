## Test types
base_test <- function(func,
                      name,
                      type = c("variable", "dataset", "metadata", "project"),
                      condition = c("err", "warn"),
                      condition_threshold = 0,
                      record_level = FALSE,
                      .class = NULL) {
  stopifnot(is.function(func))
  stopifnot(length(formals(func)) == 1)
  stopifnot(is.character(name))

  type <- match.arg(type)
  condition <- match.arg(condition)

  structure(
    func,
    name = name,
    type = type,
    condition = condition,
    condition_threshold = condition_threshold,
    record_level = record_level,
    class = c(.class, "bpr_test", class(func))
  )
}

variable_test <- function(func,
                          name,
                          condition = "err",
                          condition_threshold = 0,
                          record_level = TRUE) {
  base_test(
    func,
    name,
    type = "variable",
    condition = condition,
    condition_threshold = condition_threshold,
    record_level = record_level,
    .class = "variable_bpr_test"
  )
}

dataset_test <- function(func,
                         name,
                         condition = "err",
                         condition_threshold = 0,
                         record_level = FALSE) {
  base_test(
    func,
    name,
    type = "dataset",
    condition = condition,
    condition_threshold = condition_threshold,
    record_level = record_level,
    .class = "dataset_bpr_test"
  )
}

metadata_test <- function(func,
                          name,
                          condition = "warn",
                          condition_threshold = 0,
                          record_level = FALSE) {
  base_test(
    func,
    name,
    type = "metadata",
    condition = condition,
    condition_threshold = condition_threshold,
    record_level = record_level,
    .class = "metadata_bpr_test"
  )
}

project_test <- function(func,
                         name,
                         condition = "warn") {
  base_test(
    func,
    name,
    type = "project",
    condition = condition,
    record_level = FALSE,
    .class = "project_bpr_test"
  )
}

#' @export
print.bpr_test <- function(x, ...) {
  cat_line("<blueprintr test: '{get_attr(x, 'name')}'>")
  cat_line("type: '{get_attr(x, 'type')}'")
  cat_line("level: '{get_attr(x, 'condition')}'")
  cat_line("threshold: {get_attr(x, 'condition_threshold')}")
  cat_line("record level?: {get_attr(x, 'record_level')}")
  cat_line("function:")

  y <- x
  attributes(y) <- NULL
  print(y)

  invisible(x)
}

## Import and prepare tests

#' Thin wrapper around [rlang::enexprs()] for tests
#'
#' Captures all provided arguments as language objects
#' for later evaluation. Necessary for providing lists
#' of tests to run.
#'
#' @param ... Any test calls
#' @return A list of the calls in their unevaluated form
#' @export
test_list <- function(...) {
  rlang::enexprs(...)
}

interpret_tests <- function(x, ...) {
  UseMethod("interpret_checks", x)
}

#' @export
interpret_tests.blueprint_metadata <- function(x, ...) {
  if (!"tests" %in% names(x)) {
    return(list())
  }

  raw_tests <- get_raw_tests(x)
  lapply(raw_tests, as.call)
}

raw_test <- function(target_name, test_expr) {
  stopifnot(is.character(target_name))
  stopifnot(is.language(test_expr))
  stopifnot(is.call(test_expr))

  structure(
    list(
      target = target_name,
      call = test_expr
    ),
    class = "blueprint_raw_test"
  )
}

#' @export
as.call.blueprint_raw_test <- function(x) {
  bquote(.(x$call)(.(as.name(x$target))))
}

get_raw_tests <- function(x, ...) {
  UseMethod("get_raw_tests", x)
}

#' @export
get_raw_tests.blueprint_metadata <- function(x, ...) {
  out <- lapply(seq_len(nrow(x)), function(.i) {
    parsed_tests <- rlang::parse_expr(
      paste0("test_list(", x[["tests"]][[.i]], ")")
    )
    test_exprs <- eval(parsed_tests)

    lapply(test_exprs, function(te) raw_test(x[["name"]][[.i]], te))
  })

  flatten(out)
}

prepare_tests <- function(..., .env = parent.frame()) {
  prepared_tests <- rlang::enexprs(...)
  validate_prepared_tests(prepared_tests)

  test_objs <- lapply(
    prepared_tests,
    function(x) eval(rlang::node_car(x), envir = .env)
  )

  test_names <- vcapply(test_objs, get_attr, "name")
  test_conditions <- vcapply(test_objs, get_attr, "condition")
  test_thresholds <- vdapply(test_objs, get_attr, "condition_threshold")
  test_types <- vcapply(test_objs, get_attr, "type")
  record_levels <- vlapply(test_objs, get_attr, "record_level")
  test_targets <- vcapply(
    prepared_tests,
    function(x) as.character(rlang::node_cdr(x)[[1]])
  )

  dat <- tidytable::tidytable(
    test_type = test_types,
    test_call = prepared_tests,
    test_name = test_names,
    test_target = test_targets,
    test_condition = test_conditions,
    test_threshold = test_thresholds,
    record_level = record_levels,
  )

  structure(
    dat,
    class = c("bpr_prepared_tests", class(dat))
  )
}

validate_prepared_tests <- function(tests) {
  for (test in tests) {
    test_chr <- safe_deparse(test, collapse = " ", trim = TRUE)
    errmsg <- "Call of form `f(...)(x)` expected.\nProvided test: `{test_chr}`"

    res <- tryCatch(
      is_prepared_test(test),
      error = function(e) bp_err(errmsg)
    )
    if (!res) bp_err(errmsg)
  }

  invisible(tests)
}

#' Checks if a test call expression matches a prepared test
#'
#' Prepared text statements match the form `f(...)(x)`
#'
#' @param .call An R expression
#' @return Logical
is_prepared_test <- function(.call) {
  if (!is.call(.call)) {
    return(FALSE)
  }

  if (is_qualified_sym(.call)) {
    return(FALSE)
  }

  car <- rlang::node_car(.call)
  args <- rlang::call_args(.call)

  if (length(args) != 1L) {
    return(FALSE)
  }

  if (!is.call(car)) {
    return(FALSE)
  }

  if (is_qualified_sym(car)) {
    return(FALSE)
  }

  TRUE
}

## Run tests

run_tests <- function(context, ..., .env = parent.frame()) {
  UseMethod("run_tests", context)
}

#' @export
run_tests.default <- function(context,
                              ...,
                              .env = parent.frame()) {
  prepared_tests <- prepare_tests(..., .env = .env)
  eval_prepped_tests(prepared_tests, context, .env = .env)
}

eval_prepped_tests <- function(prepped_tests, context, .env = parent.frame()) {
  stopifnot(inherits(prepped_tests, "bpr_prepared_tests"))

  evaled_tests <- lapply(
    prepped_tests$test_call,
    rlang::eval_tidy,
    data = context,
    env = .env
  )

  validate_logical_test_values(evaled_tests, prepped_tests)
  validate_no_missing_test_values(evaled_tests, prepped_tests)

  out <- prepped_tests
  out[["results"]] <- evaled_tests
  out[["pass"]] <- rep(FALSE, nrow(out))

  for (i in seq_len(nrow(out))) {
    if (isTRUE(out$record_level[[i]])) {
      out$pass[[i]] <- mean(out$results[[i]]) >= (1 - out$test_threshold[[i]])
    } else {
      out$pass[[i]] <- all(out$results[[i]])
    }
  }

  out <- structure(
    out,
    class = c("bpr_evaled_tests", class(out))
  )

  out
}

validate_logical_test_values <- function(evaled_tests, prepped_tests) {
  stopifnot(is.list(evaled_tests))
  stopifnot(inherits(prepped_tests, "bpr_prepared_tests"))

  for (i in seq_along(evaled_tests)) {
    log_check <- is.logical(evaled_tests[[i]])

    if (!isTRUE(log_check)) {
      bp_err(c(
        "Invalid test value: test must be logical\n",
        "Test: {prepped_tests$test_name[[i]]}\n",
        "Target: {prepped_tests$test_target[[i]]}"
      ))
    }
  }

  invisible(evaled_tests)
}

validate_no_missing_test_values <- function(evaled_tests, prepped_tests) {
  stopifnot(is.list(evaled_tests))
  stopifnot(inherits(prepped_tests, "bpr_prepared_tests"))

  for (i in seq_along(evaled_tests)) {
    na_check <- all(!is.na(evaled_tests[[i]]))

    if (!isTRUE(na_check)) {
      bp_err(c(
        "Invalid test value: cannot have missing values\n",
        "Test: {prepped_tests$test_name[[i]]}\n",
        "Target: {prepped_tests$test_target[[i]]}"
      ))
    }
  }

  invisible(evaled_tests)
}