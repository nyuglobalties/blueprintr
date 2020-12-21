context("utils")

test_that("Flattening behaves correctly", {
  test <- list(
    list(
      list(a = 1, b = 3),
      list(x = 5, y = 4)
    ),

    list(
      list(u = 1, v = 3),
      list(w = 5, z = 4)
    ),

    list(
      list(thing = "stuff", yes = TRUE, path = "~"),
      list(thing = "stuff1", yes = FALSE),
      list(thing = "stuff2", yes = TRUE, path = "~/Documents")
    )
  )

  expect <- list(
    list(a = 1, b = 3),
    list(x = 5, y = 4),
    list(u = 1, v = 3),
    list(w = 5, z = 4),
    list(thing = "stuff", yes = TRUE, path = "~"),
    list(thing = "stuff1", yes = FALSE),
    list(thing = "stuff2", yes = TRUE, path = "~/Documents")
  )

  expect_identical(flatten(test), expect)
})

test_that("Custom conditions behave properly", {
  err <- expect_error(bp_err("err"))
  expect_true(inherits(err, "bp_error"))
  expect_equiv(err$message, "err")

  expect_warning(bp_warn("warn"))
  warn <- capture_warning(bp_warn("warn"))
  expect_true(inherits(warn, "bp_warning"))
  expect_equiv(warn$message, "warn")
})
