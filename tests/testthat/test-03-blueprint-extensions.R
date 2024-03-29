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

test_that("Extra bpstep additions behave correctly", {
  expect_error(bp_add_bpstep(list(), list()))

  test_bp <- blueprint(
    "test_bp_1",
    description = "dummy",
    command = mtcars,
    unknown_param = FALSE
  )

  expect_error(bp_add_bpstep(test_bp, list()))

  test_bpstep <- bpstep(
    step = "test_step",
    bp = test_bp,
    payload = bpstep_payload(
      target_name = "testy",
      target_command = quote(print("hi"))
    )
  )

  test_bp_withstep <- bp_add_bpstep(
    test_bp,
    test_bpstep
  )

  expect_false(is.null(test_bp_withstep$extra_steps))
  expect_equivalent(
    test_bp_withstep$extra_steps[[1]],
    test_bpstep
  )

  expect_error(
    bp_add_bpstep(
      test_bp_withstep,
      test_bpstep
    )
  )
})

test_that("bp_extend() and bp_add_bpstep() don't break each other", {
  test_bp <- blueprint(
    "test_bp_1",
    description = "dummy",
    command = mtcars
  )

  bp1 <- bp_label_variables(bp_export_codebook(test_bp))
  bp2 <- bp_export_codebook(bp_label_variables(test_bp))

  expect_false(is.null(bp1$extra_steps))
  expect_identical(length(bp1$extra_steps), length(bp2$extra_steps))

  cb1 <- bp1$extra_steps[[1]]
  cb2 <- bp2$extra_steps[[1]]

  expect_identical(cb1$step, "export_codebook")
  expect_identical(cb1$step, cb2$step)
})

test_that("Multiple steps make sense when applicable", {
  test_bp <- blueprint(
    "test_bp_1",
    description = "dummy",
    command = mtcars
  )

  expect_error(
    bp_export_codebook(bp_export_codebook(test_bp)),
    class = "bp_error"
  )
})