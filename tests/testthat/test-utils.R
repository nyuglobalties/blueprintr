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
