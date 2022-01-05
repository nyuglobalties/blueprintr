context("bpstep_payload")

test_that("bpstep payloads are formed correctly", {
  expect_error(bpstep_payload(1, quote("hi")))
  expect_error(bpstep_payload("hi", "hi"))

  payload1 <- bpstep_payload(
    target_name = "some_target",
    target_command = quote(print("hi"))
  )

  expect_s3_class(payload1, "bpstep_payload")
  expect_identical(payload1$target_name, "some_target")
  expect_identical(payload1$target_command, quote(print("hi")))

  payload2 <- bpstep_payload(
    target_name = "some_target",
    target_command = quote(print("hi")),
    format = "file"
  )

  expect_true("format" %in% names(payload2))
  expect_identical(payload2$format, "file")
})

test_that("'drake' blueprint step payloads are rendered correctly", {
  skip_if_not_installed("drake")

  # Basic target
  command1 <- quote(print("hi"))
  target_name1 <- "hi"

  expect1 <- drake::drake_plan(hi = print("hi"))
  payload1 <- bpstep_payload(
    target_name1,
    command1
  )

  equivalent_plans(
    assemble_payload(drake_assembler(), payload1),
    expect1
  )

  # Complex target
  command2 <- quote({print("hi"); "some/path/to/file"})
  target_name2 <- "hi_with_file"
  expect2 <- drake::drake_plan(
    hi_with_file = drake::target(
      command = {
        print("hi")
        "some/path/to/file"
      },

      format = "file"
    )
  )

  payload2 <- bpstep_payload(
    target_name2,
    command2,
    format = "file"
  )

  equivalent_plans(
    assemble_payload(drake_assembler(), payload2),
    expect2
  )
})

test_that("'targets' blueprint step payloads are rendered correctly", {
  skip_if_not_installed("targets")

  # Basic target
  command1 <- quote(print(mtcars))
  target_name1 <- "hi"

  expect1 <- targets::tar_target_raw(target_name1, command1)
  payload1 <- bpstep_payload(
    target_name1,
    command1
  )

  expect_equivalent(
    assemble_payload(targets_assembler(), payload1),
    expect1
  )

  # Complex target
  command2 <- quote({print("hi"); "some/path/to/file"})
  target_name2 <- "hi_with_file"
  expect2 <- targets::tar_target_raw(target_name2, command2, format = "file")

  payload2 <- bpstep_payload(
    target_name2,
    command2,
    format = "file"
  )

  expect_equivalent(
    assemble_payload(targets_assembler(), payload2),
    expect2
  )
})
