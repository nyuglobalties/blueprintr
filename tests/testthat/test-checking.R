context("checking")

test_that("Target content checks work", {
  is_mtcars <- function(df) {
    setequal(df, mtcars)
  }

  mtcars_bp <- blueprint(
    "mtcars_chunk",
    description = "It's just mtcars, OK?",
    checks = check_list(
      is_mtcars()
    ),
    metadata_file_path = bp_path("blueprints"),
    command = {
      df <- mtcars
      df$cyl <- df$cyl - 1
      df
    }
  )

  plan <- plan_from_blueprint(mtcars_bp)

  drake::clean()
  expect_error(drake::make(plan))

  meta <- drake::diagnose(mtcars_chunk_checks)
  expect_true(inherits(meta$error, "checks_error"))
})

test_that("Variable checks work", {
  mtcars_bp <- blueprint(
    "mtcars_vartests",
    description = "It's just mtcars, OK?",
    metadata_file_path = bp_path("blueprints"),
    command = {
      df <- mtcars
      df$cyl <- df$cyl - 1
      df
    }
  )

  plan <- plan_from_blueprint(mtcars_bp)

  drake::clean()
  expect_error(drake::make(plan))

  meta <- drake::diagnose(mtcars_vartests_checks)
  expect_true(inherits(meta$error, "checks_error"))
})
