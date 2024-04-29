test_that("Mapped DFs are interpreted correctly", {
  skip_if_not_installed("panelcleaner")

  basic_mapping_df <- tidytable::tribble(
    ~name_1, ~coding_1, ~description_1, ~panel, ~homogenized_name, ~homogenized_coding,
    "x1", NA, "A numeric variable", "example", "x1", NA,
    "x_2", "coding(code('Yes', 1), code('No', 0))", "A binary variable", "example", "x2", "coding(code('Yes', 1), code('No', 0))"
  )

  basic_mapping <- panelcleaner::panel_mapping(basic_mapping_df, 1)

  set.seed(90210)
  dat <- data.frame(x1 = rnorm(100), x_2 = as.integer(runif(100) > 0.5))
  pnl <- panelcleaner::enpanel("example", dat)
  pnl <- panelcleaner::add_mapping(pnl, basic_mapping)
  pnl <- panelcleaner::homogenize_panel(pnl)
  pnl <- panelcleaner::bind_waves(pnl)
  pnl_df <- as.data.frame(pnl)

  expect_identical(mdf_panel_mapping(pnl_df), basic_mapping)
  expect_identical(mdf_schema_panel(pnl_df), "panel")
  expect_identical(mdf_schema_homogenized_name(pnl_df), "homogenized_name")
  expect_identical(mdf_schema_homogenized_coding(pnl_df), "homogenized_coding")
})

test_that("mdf_keep_panel() works", {
  skip_if_not_installed("panelcleaner")

  basic_mapping_df <- tidytable::tribble(
    ~name_1, ~coding_1, ~description_1, ~panel, ~homogenized_name, ~homogenized_coding,
    "x1", NA, "A numeric variable", "example", "x1", NA,
    "x_2", "coding(code('Yes', 1), code('No', 0))", "A binary variable", "example", "x2", "coding(code('Yes', 1), code('No', 0))",
    "q1", NA, "Extra var in another panel", "another", "q1", NA
  )

  basic_mapping <- panelcleaner::panel_mapping(basic_mapping_df, 1)

  expect_mapping_df <- tidytable::tribble(
    ~name_1, ~coding_1, ~description_1, ~panel, ~homogenized_name, ~homogenized_coding,
    "x1", NA, "A numeric variable", "example", "x1", NA,
    "x_2", "coding(code('Yes', 1), code('No', 0))", "A binary variable", "example", "x2", "coding(code('Yes', 1), code('No', 0))"
  )

  expect_mapping <- panelcleaner::panel_mapping(expect_mapping_df, 1)

  set.seed(90210)
  dat <- data.frame(x1 = rnorm(100), x_2 = as.integer(runif(100) > 0.5))
  pnl <- panelcleaner::enpanel("example", dat)
  pnl <- panelcleaner::add_mapping(pnl, basic_mapping)
  pnl <- panelcleaner::homogenize_panel(pnl)
  pnl <- panelcleaner::bind_waves(pnl)
  pnl_df <- as.data.frame(pnl)

  subset <- mdfm_keep_panel(pnl_df)
  expect_equivalent(subset, expect_mapping)
})

test_that("mapped_df import works correctly", {
  skip_if_not_installed("panelcleaner")

  # Make sure mdf_import_meta only runs on mapped_dfs
  meta_dt <- initial_metadata_dt(datasets::mtcars)
  expect_identical(
    mdf_import_meta(meta_dt, datasets::mtcars),
    meta_dt
  )

  basic_mapping_df <- tidytable::tribble(
    ~name_1, ~coding_1, ~description_1, ~panel, ~homogenized_name, ~homogenized_coding,
    "x1", NA, "A numeric variable", "example", "x1", NA,
    "x_2", "coding(code('Yes', 1), code('No', 0))", "A binary variable", "example", "x2", "coding(code('Yes', 1), code('No', 0))",
    "q1", NA, "Extra var in another panel", "another", "q1", NA
  )

  basic_mapping <- panelcleaner::panel_mapping(basic_mapping_df, 1)

  set.seed(90210)
  dat <- data.frame(x1 = rnorm(100), x_2 = as.integer(runif(100) > 0.5))
  pnl <- panelcleaner::enpanel("example", dat)
  pnl <- panelcleaner::add_mapping(pnl, basic_mapping)
  pnl <- panelcleaner::homogenize_panel(pnl)
  pnl <- panelcleaner::bind_waves(pnl)
  pnl_df <- as.data.frame(pnl)

  meta <- mdf_import_meta(initial_metadata_dt(pnl_df), pnl_df)

  expect_true("coding" %in% names(meta))
  expect_identical(
    meta[meta$name == "x2", "coding", drop = TRUE],
    "coding(code('Yes', 1), code('No', 0))"
  )

  # Now checking to make sure that coding is overwritten correctly if
  # it exists in metadata_dt already

  new_meta_dt <- initial_metadata_dt(pnl_df)
  new_meta_dt$coding <- NA_character_

  new_meta <- mdf_import_meta(new_meta_dt, pnl_df)

  expect_true("coding" %in% names(new_meta))
  expect_true(!"pm_coding" %in% names(new_meta))

  expect_identical(
    new_meta[new_meta$name == "x2", "coding", drop = TRUE],
    "coding(code('Yes', 1), code('No', 0))"
  )
})
