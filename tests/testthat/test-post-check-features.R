context("Post-check features")

test_that("Variables are reordered and dropped correctly", {
  mtcars_rearranged_bp <- blueprint(
    "mtcars_chunk_rearranged",
    command = mtcars,
    metadata_directory = bp_path("blueprints")
  )

  plan <- plan_from_blueprint(mtcars_rearranged_bp)

  drake::clean()
  drake::make(plan)

  drake::loadd(mtcars_chunk_rearranged)

  expect_identical(
    names(mtcars_chunk_rearranged),
    c("cyl", "mpg", "disp", "hp", "drat", "wt")
  )
})
