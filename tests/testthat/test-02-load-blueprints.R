context("loading blueprints")

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
