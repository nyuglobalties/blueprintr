context("Blueprint extensions")

test_that("Basic blueprint extension system works", {
  test_bp <- blueprint(
    "test_bp",
    description = "hi",
    command = mtcars
  )

  new_bp <- bp_extend(test_bp, description = "changed")
  expect_identical(new_bp$description, "changed")

  newer_bp <- bp_extend(test_bp, new_param = "hello")
  expect_true(!is.null(newer_bp$new_param))
  expect_identical(newer_bp$new_param, "hello")

  test_bp_2 <- blueprint(
    "test_bp_1",
    description = "dummy",
    command = mtcars,
    unknown_param = FALSE
  )

  newest_bp <- bp_extend(test_bp_2, unknown_param = TRUE)
  expect_true(newest_bp$unknown_param)
})
