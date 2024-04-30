test_that("Codebook exports are added to plan correctly", {
  test_bp_0 <- blueprint(
    "test_bp",
    command = mtcars
  )
  test_bp_0 <- bp_export_codebook(test_bp_0)

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_0)
    })

    manifest <- tar_manifest_local()
    expect_true(blueprint_codebook_name(test_bp_0) %in% manifest$name)
  })

  test_bp_1 <- blueprint(
    "test_bp",
    command = mtcars
  )

  test_bp_1 <- bp_export_codebook(
    test_bp_1,
    summaries = TRUE
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_1)
    })

    manifest <- tar_manifest_local()

    render_cmd <- manifest %>%
      tidytable::filter(.data$name == blueprint_codebook_name(test_bp_1)) %>%
      tidytable::pull(.data$command) %>%
      rlang::parse_expr()

    expect_true(!is.null(render_cmd[[2]][["dataset"]]))
    expect_identical(
      as.character(render_cmd[[2]][["dataset"]]),
      blueprint_final_name(test_bp_1)
    )
  })

  random_template_location <- "some/random/rmarkdown/file.Rmd"
  test_bp_2 <- blueprint(
    "test_bp",
    command = mtcars
  )

  test_bp_2 <- bp_export_codebook(
    test_bp_2,
    template = random_template_location
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_2)
    })

    manifest <- tar_manifest_local()

    render_cmd <- manifest %>%
      tidytable::filter(.data$name == blueprint_codebook_name(test_bp_2)) %>%
      tidytable::pull(.data$command) %>%
      rlang::parse_expr()

    expect_identical(
      render_cmd[[2]][["template"]],
      bquote(knitr_in(.(random_template_location)))
    )
  })
})

test_that("Codebooks are rendered safely", {
  skip_if_not_installed("labelled")

  test_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp)
    })

    tar_make_local()

    targets::tar_load(mtcars_chunk_rearranged_blueprint)
    targets::tar_load(mtcars_chunk_rearranged_meta)
    temp_file <- file.path(tempdir(), "mtcars_chunk_rearranged.html")

    # Suppress viewing the table
    opts <- options(kableExtra_view_html = FALSE)

    render_out <- tryCatch(
      render_codebook(
        mtcars_chunk_rearranged_blueprint,
        mtcars_chunk_rearranged_meta,
        temp_file
      ),
      error = function(e) e
    )

    expect_true(!inherits(render_out, "error"))
    options(opts)
  })
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

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_with_cbexport)
    })
    manifest <- tar_manifest_local()
    expect_true("mtcars_chunk_rearranged_codebook" %in% manifest$name)
  })

  # Adding custom args
  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    summaries = TRUE
  )

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    file = here::here("testy.html")
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_with_cbexport)
    })
    manifest <- tar_manifest_local()
    expect_true("mtcars_chunk_rearranged_codebook" %in% manifest$name)
  })

  test_bp_with_cbexport <- bp_export_codebook(
    test_bp,
    title = "test"
  )

  targets::tar_dir({
    targets::tar_script({
      blueprintr::tar_blueprint_raw(test_bp_with_cbexport)
    })
    manifest <- tar_manifest_local()
    expect_true("mtcars_chunk_rearranged_codebook" %in% manifest$name)
  })
})
