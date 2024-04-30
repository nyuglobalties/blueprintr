test_that("Variables are reordered and dropped correctly", {
  mtcars_rearranged_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_rearranged_bp)
    })

    tar_make_local()

    targets::tar_load(mtcars_chunk_rearranged)
    expect_identical(
      names(mtcars_chunk_rearranged),
      c("cyl", "mpg", "disp", "hp", "drat", "wt")
    )
  })
})

test_that("Variables are converted to labelled vectors correctly", {
  skip_if_not_installed("labelled")

  mtcars_rearranged_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints"),
    labelled = TRUE
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_rearranged_bp)
    })

    tar_make_local()

    targets::tar_load(mtcars_chunk_rearranged)

    expect_true(labelled::is.labelled(mtcars_chunk_rearranged$cyl))
    # mpg is _not_ "labelled" because it has double values

    variable_title <- function(x) {
      attr(x, "label", exact = TRUE)
    }

    variable_levels <- function(x) {
      attr(x, "labels", exact = TRUE)
    }

    expect_identical(variable_title(mtcars_chunk_rearranged$cyl), "Number of cylinders")
    expect_identical(variable_title(mtcars_chunk_rearranged$mpg), "Gas mileage")

    expect_equivalent(variable_levels(mtcars_chunk_rearranged$cyl), c(Four = 4, Six = 6, Eight = 8))
  })
})

test_that("Variables are annotated correctly", {
  mtcars_rearranged_bp <- blueprint(
    "mtcars_chunk_rearranged_copy",
    command = mtcars,
    metadata_directory = bp_path("blueprints"),
    annotate = TRUE
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_rearranged_bp)
    })

    tar_make_local()

    targets::tar_load(mtcars_chunk_rearranged_copy)

    expect_true(
      has_annotation(mtcars_chunk_rearranged_copy$cyl, "description")
    )

    expect_true(
      has_annotation(mtcars_chunk_rearranged_copy$mpg, "description")
    )

    expect_true(
      has_annotation(mtcars_chunk_rearranged_copy$cyl, "coding")
    )

    expect_false(
      has_annotation(mtcars_chunk_rearranged_copy$cyl, ".parsed_tests")
    )
  })
})
