context("blueprints")

test_that("blueprint tests are run", {
  targets::tar_dir({
    mtcars_bp <- blueprint(
      "mtcars_chunk",
      command = {
        mtcars
      },
      metadata_directory = bp_path("blueprints")
    )

    targets::tar_script({
      blueprintr::tar_blueprint_raw(mtcars_bp)
    })

    expect_message(tar_make_local())

    bad_mtcars_bp <- blueprint(
      "bad_mtcars_chunk",
      command = {
        df <- .TARGET("mtcars_chunk")
        df$mpg <- NULL
        df
      },
      metadata_file_path =
        file.path(
          bp_path("blueprints"),
          "mtcars_chunk.csv"
        )
    )

    targets::tar_script(
      {
        list(
          blueprintr::tar_blueprint_raw(mtcars_bp),
          blueprintr::tar_blueprint_raw(bad_mtcars_bp)
        )
      },
      ask = FALSE
    )

    expect_error(tar_make_local())
  })
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
    metadata_directory = bp_path("blueprints")
  )

  student_demo_bp <- blueprint(
    "student_demographics",
    description = "Some demographics for students",
    command = {
      ids <- .TARGET("id_vars")

      demos <- tidytable::tidytable(
        student_id = c("ST5402", "ST4910", "ST2819"),
        age = c(8, 10, 9),
        grade = c(4, 5, 5)
      )

      demos %>%
        tidytable::left_join(
          ids %>%
            tidytable::select(student_id, classroom_id),
          by = "student_id"
        )
    },
    metadata_directory = bp_path("blueprints")
  )

  if (metadata_file_exists(student_demo_bp)) {
    unlink(metadata_path(student_demo_bp))
  }

  expect_identical(blueprint_target_deps(id_bp), character())
  expect_identical(blueprint_target_deps(student_demo_bp), "id_vars")

  targets::tar_dir({
    targets::tar_script({
      list(
        blueprintr::tar_blueprint_raw(id_bp),
        blueprintr::tar_blueprint_raw(student_demo_bp)
      )
    })

    tar_make_local()

    student_demo_meta <- targets::tar_read(student_demographics_meta)
    id_vars_meta <- targets::tar_read(id_vars_meta)

    expect_identical(
      student_demo_meta %>%
        tidytable::filter(.data$name == "student_id") %>%
        tidytable::pull(.data$description),
      id_vars_meta %>%
        tidytable::filter(.data$name == "student_id") %>%
        tidytable::pull(.data$description)
    )
  })
})
