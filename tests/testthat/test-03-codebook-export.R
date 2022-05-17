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
    command = mtcars
  )

  test_bp_1 <- bp_export_codebook(
    test_bp_1,
    summaries = TRUE
  )

  plan <- plan_from_blueprint(test_bp_1)

  render_cmd <-
    plan %>%
    dplyr::filter(.data$target == blueprint_codebook_name(test_bp_1)) %>%
    dplyr::pull(.data$command) %>%
    `[[`(1)

  expect_true(!is.null(render_cmd[[2]][["dataset"]]))
  expect_identical(
    as.character(render_cmd[[2]][["dataset"]]),
    blueprint_final_name(test_bp_1)
  )

  random_template_location <- "some/randsome/rmarkdown/file.Rmd"
  test_bp_2 <- blueprint(
    "test_bp",
    command = mtcars
  )

  test_bp_2 <- bp_export_codebook(
    test_bp_2,
    template = random_template_location
  )

  plan <- plan_from_blueprint(test_bp_2)

  render_cmd <-
    plan %>%
    dplyr::filter(.data$target == blueprint_codebook_name(test_bp_1)) %>%
    dplyr::pull(.data$command) %>%
    `[[`(1)

  expect_identical(
    render_cmd[[2]][["template"]],
    bquote(knitr_in(.(random_template_location)))
  )
})

test_that("Codebooks are rendered safely", {
  skip_if_not_installed("labelled")

  test_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  plan <- plan_from_blueprint(test_bp)

  drake::clean()
  drake::make(plan)

  drake::loadd(mtcars_chunk_rearranged_blueprint)
  drake::loadd(mtcars_chunk_rearranged_meta)

  temp_file <- file.path(tempdir(), "mtcars_chunk_rearranged.html")

  render_out <- tryCatch(
    render_codebook(
      mtcars_chunk_rearranged_blueprint,
      mtcars_chunk_rearranged_meta,
      temp_file
    ),
    error = function(e) e
  )

  expect_true(!inherits(render_out, "error"))
  unlink(temp_file)
})

test_that("Codebook export step added correctly", {
  test_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp
  )

  plan <- plan_from_blueprint(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% plan$target)

  # Adding custom args
  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    summaries = TRUE
  )

  plan <- plan_from_blueprint(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% plan$target)

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    file = here::here("testy.html")
  )

  plan <- plan_from_blueprint(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% plan$target)

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    title = "test"
  )

  plan <- plan_from_blueprint(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% plan$target)
})

test_that("targets attachment works", {
  skip_if_not_installed("targets")

  test_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp
  )

  tar_list <- tar_blueprint_raw(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% vcapply(tar_list, function(x) x$settings$name)) # nolint

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    summaries = TRUE
  )

  tar_list <- tar_blueprint_raw(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% vcapply(tar_list, function(x) x$settings$name)) # nolint

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    file = here::here("testy.html")
  )

  tar_list <- tar_blueprint_raw(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% vcapply(tar_list, function(x) x$settings$name)) # nolint

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    title = "testy"
  )

  tar_list <- tar_blueprint_raw(test_bp_with_cbexport)
  expect_true("mtcars_chunk_rearranged_codebook" %in% vcapply(tar_list, function(x) x$settings$name)) # nolint
})