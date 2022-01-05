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

test_that("Assembled bpsteps are generated correctly", {
  dummy_bp <- blueprint("hi", print("hi"))
  step <- bpstep(
    "a_step",
    dummy_bp,
    bpstep_payload(
      "hi",
      dummy_bp$command
    )
  )

  assembled_step <- assemble_bpstep(targets_assembler(), step)
  expect_s3_class(assembled_step, "assembled_bpstep")

  target <- targets::tar_target_raw(
    "hi",
    dummy_bp$command
  )

  expect_equivalent(
    assembled_step$built_payload,
    target
  )
})
