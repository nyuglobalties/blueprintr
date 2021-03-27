test_that("Annotations behave as expected", {
  set.seed(42912)

  x <- sample(0:3, 100, replace = TRUE)

  expect_null(annotations(x))
  x <- set_attrs(x, some_attrib = "thing")
  expect_null(annotations(x))

  x <- c(x)

  expect_false(has_annotation(x, "field"))
  x <- add_annotation(x, "field", 1)

  expect_identical(annotation(x, "field"), 1)

  # Don't overwrite annotations unless explicit about it
  x <- add_annotation(x, "field", 2)
  expect_identical(annotation(x, "field"), 1)

  x <- add_annotation(x, "field", 2, TRUE)
  expect_identical(annotation(x, "field"), 2)

  expect_identical(annotations(x), list(field = 2))

  dec_table <- annotation_table(x, "x")
  expect_equal(
    dec_table,
    dplyr::tribble(
      ~name, ~field,
        "x",      2
    )
  )
})

test_that("Mutate synonyms behave as expected", {
  dat <- mutate_annotation(
    mtcars, "field",
    cyl = sum(1:5), mpg = "value",
    vs = sum(vs), wt = mean(wt)
  )

  expect_error(mutate_annotation(dat, c("wrong", "style"), cyl = "thing"))
  err <- expect_error(mutate_annotation(dat, "field", baz = "thing"))

  expect_equal(
    err$message,
    "'baz' not found in `dat`. Cannot modify annotation 'field' on it."
  )

  expect_true(has_annotation(dat$mpg, "field"))
  expect_true(has_annotation(dat$cyl, "field"))
  expect_true(has_annotation(dat$vs, "field"))
  expect_true(has_annotation(dat$wt, "field"))

  expect_identical(annotation(dat$wt, "field"), mean(mtcars$wt))
  expect_identical(annotation(dat$vs, "field"), sum(mtcars$vs))

  dat2 <- mutate_annotation_across(
    mtcars, "means", .fn = mean
  )

  for (vn in names(dat2)) {
    expect_true(has_annotation(dat2[[vn]], "means"))
    expect_identical(annotation(dat2[[vn]], "means"), mean(dat2[[vn]]))
  }
})