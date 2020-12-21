test_that("bpstep objects are formed correctly", {
  dummy_bp <- blueprint("hi", print("hi"))

  expect_error(bpstep(NULL, dummy_bp, list()))     # `step` must be character
  expect_error(bpstep("some_step", NULL, list()))  # `bp` must be a blueprint

  step <- bpstep("a_step", dummy_bp, list())
  expect_identical(step$step, "a_step")
  expect_identical(step$blueprint, dummy_bp)
  expect_identical(step$payload, list())
  expect_s3_class(step, "bpstep")
})

test_that("bpstep implmentations render correctly", {
  dummy_bp <- blueprint("hi", print("hi"))

  dbp <- drake_bpstep("a_step", dummy_bp, list())
  tbp <- targets_bpstep("a_step", dummy_bp, list())

  expect_s3_class(dbp, "drake_bpstep")
  expect_s3_class(tbp, "targets_bpstep")
})
