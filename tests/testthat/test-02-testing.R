test_that("Testing functions have correct spec", {
  # Output tests can only have arity of 1
  expect_error(variable_test(function(x, y) print(x), "test"))

  # Can only have 'err' or 'warn'
  expect_error(variable_test(function(x) print(x), "test", condition = "msg"))
})

test_that("Raw tests are rendered correctly", {
  # Language objects must be passed in, not function references
  expect_error(raw_test("target_var", print))

  # Languages objs must be calls, not symbols
  expect_error(raw_test("some_var", quote(var_is_unique)))

  rt <- raw_test("some_var", quote(var_is_unique()))
  test_call <- as.call(rt)

  expect_identical(
    test_call,
    quote(var_is_unique()(some_var))
  )
})

test_that("Tests are prepared correctly", {
  var_is_unique <- function(condition = "err") {
    f <- function(x) {
      !(duplicated(x) | duplicated(x, fromLast = TRUE))
    }

    variable_test(f, name = "unique", condition = condition)
  }

  var_is_not_null <- function(condition = "err") {
    variable_test(
      function(x) !is.na(x),
      name = "not-null",
      condition = condition
    )
  }

  # Tests must be of form `f(...)(x)`
  expect_error(prepare_tests(package::thing()), class = "bp_error")
  expect_error(prepare_tests(package::thing), class = "bp_error")
  expect_error(prepare_tests(var_is_not_null()))
  expect_error(prepare_tests(var_is_not_null(), var_is_unique()))
  expect_error(prepare_tests(var_is_not_null()(x, y)))

  prepped_tests <- prepare_tests(
    var_is_not_null()(x),
    var_is_unique()(x)
  )

  expect_s3_class(prepped_tests, "bpr_prepared_tests")
  expect_true(all(c("test_type", "test_call", "test_name", "test_target") %in% names(prepped_tests))) # nolint
})

test_that("Tests run correctly on data", {
  var_is_unique <- function(condition = "err") {
    f <- function(x) {
      !(duplicated(x) | duplicated(x, fromLast = TRUE))
    }

    variable_test(f, name = "unique", condition = condition)
  }

  var_is_not_null <- function(condition = "err") {
    variable_test(
      function(x) !is.na(x),
      name = "not-null",
      condition = condition
    )
  }

  bad_test_chr <- function() {
    variable_test(
      function(x) as.character(x),
      name = "bad-test-that-generates-strings"
    )
  }

  bad_test_missing <- function() {
    variable_test(
      function(x) {
        out <- !is.na(x)
        out[sample(length(out), 3)] <- NA
        out
      },
      name = "bad-test-that-generates-nulls"
    )
  }

  dat <- tidytable::as_tidytable(mtcars)
  dat[["model_name"]] <- row.names(mtcars)

  err <- expect_error(run_tests(dat, bad_test_chr()(model_name)))
  expect_true(grepl("Invalid test value\\: test must be logical", err$message))

  err <- expect_error(run_tests(dat, bad_test_missing()(model_name)))
  expect_true(grepl("Invalid test value\\: cannot have missing values", err$message))

  res <- run_tests(
    dat,
    var_is_not_null()(model_name),
    var_is_unique()(model_name)
  )
})