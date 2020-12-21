test_that("Assembler generation works", {
  expect_true(inherits(drake_assembler(), "bp_assembler"))
  expect_true(inherits(targets_assembler(), "bp_assembler"))

  expect_true(inherits(drake_assembler(), "drake_assembler"))
  expect_true(inherits(targets_assembler(), "targets_assembler"))

  expect_equivalent(unclass(drake_assembler()), "drake")
  expect_equivalent(unclass(targets_assembler()), "targets")
})
