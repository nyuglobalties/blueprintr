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

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_bp)
    })
    expect_error(
      tar_make_local(),
      class = "checks_error"
    )
  })
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

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_bp)
    })
    expect_error(
      tar_make_local(),
      class = "checks_error"
    )
  })
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

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_bp)
    })
    err <- expect_error(tar_make_local())
    expect_true(any(grepl("  \\* ", err$message))) # Embeds reasons into err message
  })

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

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_bp)
    })
    # Allow passing with warning messages!
    expect_warning(tar_make_local())
  })
})
