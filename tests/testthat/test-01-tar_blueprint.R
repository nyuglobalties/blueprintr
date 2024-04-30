test_that("Blueprint targets render correctly", {
  dummy_bp <- blueprint("hi", print("hi"))

  expect_equiv(tar_blueprint("hi", print("hi")), tar_blueprint_raw(dummy_bp))
})

test_that("Loading blueprints from folder works correctly", {
  # As of 0.2.3, this will be relegated to an optional warning
  opts <- options(blueprintr.warn_empty_blueprints_dirs = TRUE)
  on.exit(options(opts))
  no_bps <- expect_warning(tar_blueprints(tempdir()))

  expect_identical(no_bps, list())

  expected_targets <- tar_blueprint(
    "test_blueprint",
    description = "This is a test",
    command = mtcars
  )
  expect_equiv_target_set(
    tar_blueprints(bp_path("blueprints")),
    expected_targets
  )
})
