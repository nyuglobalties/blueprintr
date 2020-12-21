equivalent_plans <- function(out, exp) {
  assert_pkg("testthat")
  out <- deparse_lang_cols(out)
  exp <- deparse_lang_cols(exp)
  out <- out[order(out$target), ]
  exp <- exp[order(exp$target), ]

  for (col in lang_cols(out)) {
    testthat::expect_equal(
      unname(unclass(out[[col]])),
      unname(unclass(exp[[col]]))
    )
  }

  for (col in setdiff(colnames(out), lang_cols(out))) {
    testthat::expect_equal(
      unname(out[[col]]),
      unname(exp[[col]])
    )
  }
}

target_set_names <- function(obj) {
  vcapply(obj, function(o) o$settings$name)
}

expect_equiv_target_set <- function(object, expected) {
  expect_true(is.list(object))
  expect_true(!inherits(object, "tar_target"))

  expect_true(length(object) == length(expected))
  expect_true(setequal(target_set_names(object), target_set_names(expected)))
}
