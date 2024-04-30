test_that("kfa report steps have correct names", {
  test_bp_0 <- blueprint(
    "test_bp",
    description = "dummy",
    command = mtcars
  )

  test_bp_withkfa <- bp_export_kfa_report(
    test_bp_0,
    scale = "Scale with Spaces and Odd-Symbols"
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_withkfa)
    })

    manifest <- tar_manifest_local()

    # snake_case translation successful
    expect_true(
      "test_bp_scale_with_spaces_and_odd_symbols_kfa_report" %in% manifest$name
    )

    cmd <- manifest %>%
      tidytable::filter(.data$name == "test_bp_scale_with_spaces_and_odd_symbols_kfa_report") %>%
      tidytable::pull("command") %>%
      rlang::parse_expr()

    expect_identical(cmd[["scale"]], "Scale with Spaces and Odd-Symbols")
  })

  test_bp_withkfas <- bp_export_kfa_report(
    test_bp_0,
    scale = c("Measure A", "Measure B")
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_withkfas)
    })

    manifest <- tar_manifest_local()

    expect_true(all(
      c(
        "test_bp_measure_a_kfa_report",
        "test_bp_measure_b_kfa_report"
      ) %in% manifest$name
    ))
  })
})

test_that("kfa report custom paths rendered correctly", {
  test_bp_0 <- blueprint(
    "test_bp",
    description = "dummy",
    command = mtcars
  )

  # Using path_pattern first
  test_bp_withkfas <- bp_export_kfa_report(
    test_bp_0,
    scale = c("Measure A", "Measure B"),
    path_pattern = "report-{snakecase_scale}.html"
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_withkfas)
    })

    manifest <- tar_manifest_local()

    cmd1 <- manifest %>%
      tidytable::filter(.data$name == "test_bp_measure_a_kfa_report") %>%
      tidytable::pull("command") %>%
      rlang::parse_expr()

    cmd2 <- manifest %>%
      tidytable::filter(.data$name == "test_bp_measure_b_kfa_report") %>%
      tidytable::pull("command") %>%
      rlang::parse_expr()

    expect_identical(cmd1[["path_pattern"]], "report-{snakecase_scale}.html")
    expect_identical(cmd2[["path_pattern"]], "report-{snakecase_scale}.html")
  })
})

test_that("kfa argument forwarding works", {
  test_bp_0 <- blueprint(
    "test_bp",
    description = "dummy",
    command = mtcars
  )

  # Using path_pattern first
  test_bp_withkfas <- bp_export_kfa_report(
    test_bp_0,
    scale = c("Measure A", "Measure B"),
    kfa_args = list(ordered = TRUE, k = 3)
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_withkfas)
    })

    manifest <- tar_manifest_local()

    cmd1 <- manifest %>%
      tidytable::filter(.data$name == "test_bp_measure_a_kfa_report") %>%
      tidytable::pull("command") %>%
      rlang::parse_expr()

    cmd2 <- manifest %>%
      tidytable::filter(.data$name == "test_bp_measure_b_kfa_report") %>%
      tidytable::pull("command") %>%
      rlang::parse_expr()

    expect_identical(cmd1[["k"]], cmd2[["k"]]) # Scales get same set of args for kfa
    expect_identical(cmd1[["k"]], 3)
    expect_true(cmd1[["ordered"]])
  })
})

# Emulating issue ID'd in Peru pipeline
# turns out this error I was testing was in targets 0.10.0: ropensci/targets#758
test_that("targets accepts kfa target names", {
  scales <- c(
    "Self-Regulation",
    "Self-Regulated Learning",
    "Child Internalizing",
    "Child Externalizing",
    "Child Relationship with Caregiver",
    "Enumerator-report Child Regulation"
  )

  test_bp <- blueprint(
    "panel4_c4",
    description = "dummy",
    command = mtcars
  )

  test_bp_withkfa <- bp_export_kfa_report(
    test_bp,
    scale = scales
  )

  expect_silent(pipeline <- tar_blueprint_raw(test_bp_withkfa))

  # Try composing extra steps
  test_bp_composed <- bp_label_variables(test_bp)
  test_bp_composed <- bp_export_codebook(test_bp_composed)
  test_bp_composed <- bp_export_kfa_report(test_bp_composed, scale = scales)

  expect_silent(pipeline <- tar_blueprint_raw(test_bp_composed))
})

