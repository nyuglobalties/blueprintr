test_that("Codebook exports are added to plan correctly", {
  test_bp <- blueprint(
    "test_bp",
    command = mtcars,
    codebook_export = TRUE
  )

  plan <- plan_from_blueprint(test_bp)

  expect_true(blueprint_codebook_name(test_bp) %in% plan$target)

  test_bp_1 <- blueprint(
    "test_bp",
    command = mtcars,
    codebook_export = TRUE,
    codebook_summaries = TRUE
  )

  plan <- plan_from_blueprint(test_bp_1)

  render_cmd <-
    plan %>%
    dplyr::filter(.data$target == blueprint_codebook_name(test_bp_1)) %>%
    dplyr::pull(.data$command) %>%
    `[[`(1)

  expect_true(!is.null(render_cmd[["dataset"]]))
  expect_identical(
    as.character(render_cmd[["dataset"]]),
    blueprint_final_name(test_bp_1)
  )

  random_template_location <- "some/randsome/rmarkdown/file.Rmd"
  test_bp_2 <- blueprint(
    "test_bp",
    command = mtcars,
    codebook_export = TRUE,
    codebook_template = random_template_location
  )

  plan <- plan_from_blueprint(test_bp_2)

  render_cmd <-
    plan %>%
    dplyr::filter(.data$target == blueprint_codebook_name(test_bp_1)) %>%
    dplyr::pull(.data$command) %>%
    `[[`(1)

  expect_identical(
    render_cmd[["template"]],
    bquote(knitr_in(.(random_template_location)))
  )
})
