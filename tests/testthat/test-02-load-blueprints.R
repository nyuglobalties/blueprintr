context("loading blueprints")

test_that("File importing works correctly", {
  expect_bp <- blueprint(
    "test_blueprint",
    description = "This is a test",
    command = mtcars
  )

  bp <- import_blueprint_file(bp_path("blueprints/test_blueprint.R"))
  expect_equiv(expect_bp, bp)
})

test_that("blueprint file fetching works correctly", {
  expect_error(fetch_blueprint_files(bp_path("asdfjashdfajksdfh")))

  # As of 0.2.3, this will be relegated to an optional warning
  opts <- options(blueprintr.warn_empty_blueprints_dirs = TRUE)
  on.exit(options(opts))
  no_bps <- expect_warning(fetch_blueprint_files(tempdir()))

  expect_null(no_bps)

  expect_bp <- blueprint(
    "test_blueprint",
    description = "This is a test",
    command = mtcars
  )

  bp_files <- fetch_blueprint_files(bp_path("blueprints"))
  expect_true(is.character(bp_files))

  bps <- lapply(bp_files, import_blueprint_file)

  expect_true(!inherits(bps, "blueprint"))
  expect_true(all(vlapply(bps, is_blueprint)))

  expect_equiv(bps[[1]], expect_bp)
})

test_that("Loading from file works", {
  plan <- mock_drake_plan(dummy = 1:5)

  plan <- load_blueprint(plan, bp_path("blueprints/test_blueprint.R"))

  expect_true("test_blueprint_initial" %in% plan$target)
  expect_true("test_blueprint" %in% plan$target)
})

test_that("Loading from directory works", {
  plan <- mock_drake_plan(dummy = 1:5)
  plan <- load_blueprints(plan, directory = bp_path("blueprints"))

  expect_true("test_blueprint_initial" %in% plan$target)
  expect_true("test_blueprint" %in% plan$target)
})

test_that("Recursively loading from directory works", {
  plan <- mock_drake_plan(dummy = 1:5)
  plan <- load_blueprints(
    plan,
    directory = bp_path("blueprints"),
    recurse = TRUE
  )

  expect_true("test_blueprint_initial" %in% plan$target)
  expect_true("test_blueprint" %in% plan$target)
  expect_true("test_subdir_blueprint_initial" %in% plan$target)
  expect_true("test_subdir_blueprint" %in% plan$target)
})

test_that("Local metadata works", {
  opt_old <- options(blueprintr.use_local_metadata_path = TRUE)
  on.exit(options(opt_old))

  dirs <- load_dirs_recurse(bp_path("blueprints"), recurse = TRUE)
  bp_list <- fetch_blueprints_from_dir(dirs)

  subdir_bp <- vlapply(bp_list, function(bp) bp$name == "test_subdir_blueprint")
  subdir_bp <- bp_list[subdir_bp][[1]]

  expect_identical(
    subdir_bp$metadata_file_path,
    file.path(
      bp_path("blueprints", "subdir_test"),
      "test_subdir_blueprint.csv"
    )
  )

  subdir_bp2 <- vlapply(bp_list, function(bp) bp$name == "test_subdir_blueprint2")
  subdir_bp2 <- bp_list[subdir_bp2][[1]]

  expect_identical(
    subdir_bp2$metadata_file_path,
    file.path(
      bp_path("blueprints", "subdir_test"),
      "test2.csv"
    )
  )
})
