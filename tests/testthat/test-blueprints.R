context("blueprints")

test_that("blueprints are attached to drake plans", {
  initial_plan <- drake::drake_plan(
    loaded_data = data.table::fread("test/path.csv")
  )

  test_bp <- blueprint(
    "wide_data",
    command = tidyr::pivot_longer(
      loaded_data,
      dplyr::starts_with("ID_")
    )
  )

  plan <- attach_blueprints(initial_plan, test_bp)

  mock_plan <- dplyr::tribble(
    ~ target, ~ command,
    "loaded_data", rlang::expr(data.table::fread("test/path.csv")),
    "wide_data", rlang::expr(tidyr::pivot_longer(loaded_data, dplyr::starts_with("ID_"))),
    "wide_data_metadata_export", rlang::expr(create_metadata_file(wide_data, !!metadata_path(test_bp)))
  )

  equivalent_plans(plan, mock_plan)

  mtcars_bp <- blueprint(
    "mtcars_chunk",
    command = { mtcars },
    metadata_file_path = bp_path("blueprints")
  )
})

test_that("blueprint tests are run", {
  mtcars_bp <- blueprint(
    "mtcars_chunk",
    command = { mtcars },
    metadata_file_path = bp_path("blueprints")
  )

  plan <- plan_from_blueprint(mtcars_bp)

  drake::clean()
  expect_message(drake::make(plan))

  bad_mtcars_bp <- blueprint(
    "bad_mtcars_chunk",
    command = { 
      df <- .CHUNK("mtcars_chunk")
      df$mpg <- NULL
      df
    },
    metadata_file_path = bp_path("blueprints")
  )

  plan <- attach_blueprints(plan, bad_mtcars_bp)

  drake::clean()
  expect_error(drake::make(plan))
})
