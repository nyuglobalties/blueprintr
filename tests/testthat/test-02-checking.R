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
    metadata_directory = bp_path("blueprints"),
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
    metadata_directory = bp_path("blueprints"),
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

test_that("Content checking embeds extra information if desired", {
  mtcars_bp <- blueprint(
    "mtcars_vartests",
    description = "It's just mtcars, OK?",
    metadata_directory = bp_path("blueprints"),
    command = {
      df <- mtcars
      tidytable::rename(df, ma = am)
    }
  )

  plan <- plan_from_blueprint(mtcars_bp)

  drake::clean()
  err <- expect_error(drake::make(plan))
  expect_true(any(grepl("  \\* ", err$message))) # Embeds reasons into err message

  mtcars_bp <- blueprint(
    "mtcars_vartests",
    description = "It's just mtcars, OK?",
    metadata_directory = bp_path("blueprints"),
    command = {
      df <- mtcars
      df$new_col <- 0
      df
    }
  )

  plan <- plan_from_blueprint(mtcars_bp)

  drake::clean()
  expect_warning(drake::make(plan))

  meta <- drake::diagnose(mtcars_vartests_checks)
  expect_true(any(grepl("  \\* ", meta$warnings))) # Allow passing with warning messages!
})
