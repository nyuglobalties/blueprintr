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
      df <- .TARGET("mtcars_chunk")
      df$mpg <- NULL
      df
    },
    metadata_file_path = bp_path("blueprints")
  )

  plan <- attach_blueprints(plan, bad_mtcars_bp)

  drake::clean()
  expect_error(drake::make(plan))
})

test_that("Dependencies are handled properly", {
  id_bp <- blueprint(
    "id_vars",
    description = "Dataset that contains all ID variables for relations",
    command = data.frame(
      student_id = c("ST5402", "ST4910", "ST2819"),
      teacher_id = c("RT0014", "RT0013", "RT0013"),
      school_id = c("SC01", "SC01", "SC01"),
      classroom_id = c("RC0011", "RC0012", "RC0012"),
      stringsAsFactors = FALSE
    ),
    metadata_file_path = bp_path("blueprints")
  )

  student_demo_bp <- blueprint(
    "student_demographics",
    description = "Some demographics for students",
    command = {
      ids <- .TARGET("id_vars")
      id_dt <- as.data.table(ids)

      demos <- data.frame(
        student_id = c("ST5402", "ST4910", "ST2819"),
        age = c(8, 10, 9),
        grade = c(4, 5, 5),
        stringsAsFactors = FALSE
      )

      demos_dt <- as.data.table(demos)
      demos_dt[, classroom_id := id_dt[.SD, classroom_id, on = "student_id"]]
      as.data.frame(demos_dt)
    },
    metadata_file_path = bp_path("blueprints")
  )

  if (metadata_file_exists(student_demo_bp)) {
    unlink(metadata_path(student_demo_bp))
  }

  expect_identical(blueprint_deps(student_demo_bp), "id_vars")

  plan <- plan_from_blueprint(id_bp) %>%
    attach_blueprint(student_demo_bp)

  drake::clean()
  drake::make(plan)
})
