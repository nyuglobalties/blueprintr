test_that("Assembler generation works", {
  expect_true(inherits(drake_assembler(), "bp_assembler"))
  expect_true(inherits(targets_assembler(), "bp_assembler"))

  expect_true(inherits(drake_assembler(), "drake_assembler"))
  expect_true(inherits(targets_assembler(), "targets_assembler"))

  expect_equivalent(unclass(drake_assembler()), "drake")
  expect_equivalent(unclass(targets_assembler()), "targets")
})

test_that("Assembly steps generated correctly", {
  test_bp <- blueprint("testy", mtcars)

  steps <- default_assembly_steps(test_bp)
  stepnames <- vcapply(steps, function(s) s$step)

  assembled_steps <- assembly_steps(
    targets_assembler(),
    test_bp
  )

  assembled_stepnames <- vcapply(
    assembled_steps,
    function(s) s$step
  )

  expect_setequal(stepnames, assembled_stepnames)

  stepnames <- c(stepnames, "export_codebook")

  assembled_steps <- assembly_steps(
    targets_assembler(),
    bp_export_codebook(
      test_bp
    )
  )

  assembled_stepnames <- vcapply(
    assembled_steps,
    function(s) s$step
  )

  expect_setequal(stepnames, assembled_stepnames)
})