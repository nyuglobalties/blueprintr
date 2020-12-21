context("loading blueprints")

test_that("File importing works correctly", {
  expect_bp <- blueprint(
    "test_blueprint",
    description = "This is a test",
    command = mtcars
  )

  bp <- import_blueprint_file(bp_path("blueprints/test_blueprint.R"))
  expect_equiv(expect_bp, bp)
})

test_that("blueprint file fetching works correctly", {
  expect_error(fetch_blueprint_files(bp_path("asdfjashdfajksdfh")))

  no_bps <- expect_warning(fetch_blueprint_files(tempdir()))
  expect_null(no_bps)

  expect_bp <- blueprint(
    "test_blueprint",
    description = "This is a test",
    command = mtcars
  )

  bps <- fetch_blueprint_files(bp_path("blueprints"))
  expect_true(is.list(bps))
  expect_true(!inherits(bps, "blueprint"))
  expect_true(all(vlapply(bps, is_blueprint)))

  expect_equiv(bps[[1]], expect_bp)
})

test_that("Loading from file works", {
  plan <- drake::drake_plan(dummy = 1:5)

  plan <-
    plan %>%
    load_blueprint(bp_path("blueprints/test_blueprint.R"))

  expect_true("test_blueprint_initial" %in% plan$target)
  expect_true("test_blueprint" %in% plan$target)
})

test_that("Loading from directory works", {
  plan <- drake::drake_plan(dummy = 1:5)

  plan <-
    plan %>%
    load_blueprints(directory = bp_path("blueprints"))

  expect_true("test_blueprint_initial" %in% plan$target)
  expect_true("test_blueprint" %in% plan$target)
})
