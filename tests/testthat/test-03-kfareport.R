test_that("kfa report steps have correct names", {
  test_bp <- blueprint(
    "test_bp",
    description = "dummy",
    command = mtcars
  )

  test_bp_withkfa <- bp_export_kfa_report(
    test_bp,
    scale = "Scale with Spaces and Odd-Symbols"
  )

  plan <- plan_from_blueprint(test_bp_withkfa)

  # snake_case translation successful
  expect_true(
    "test_bp_scale_with_spaces_and_odd_symbols_kfa_report" %in% plan$target
  )

  cmd <- plan[
    plan[["target"]] == "test_bp_scale_with_spaces_and_odd_symbols_kfa_report",
    "command",
    drop = TRUE
  ][[1]]

  expect_identical(cmd[["scale"]], "Scale with Spaces and Odd-Symbols")

  test_bp_withkfas <- bp_export_kfa_report(
    test_bp,
    scale = c("Measure A", "Measure B")
  )

  plan <- plan_from_blueprint(test_bp_withkfas)

  expect_true(all(
    c("test_bp_measure_a_kfa_report", "test_bp_measure_b_kfa_report") %in% plan$target # nolint
  ))
})

test_that("kfa report custom paths rendered correctly", {
  test_bp <- blueprint(
    "test_bp",
    description = "dummy",
    command = mtcars
  )

  # Using path_pattern first
  test_bp_withkfas <- bp_export_kfa_report(
    test_bp,
    scale = c("Measure A", "Measure B"),
    path_pattern = "report-{snakecase_scale}.html"
  )

  plan <- plan_from_blueprint(test_bp_withkfas)

  cmd1 <- plan[
    plan[["target"]] == "test_bp_measure_a_kfa_report",
    "command",
    drop = TRUE
  ][[1]]

  cmd2 <- plan[
    plan[["target"]] == "test_bp_measure_b_kfa_report",
    "command",
    drop = TRUE
  ][[1]]

  expect_identical(cmd1[["path_pattern"]], "report-{snakecase_scale}.html")
  expect_identical(cmd2[["path_pattern"]], "report-{snakecase_scale}.html")
})