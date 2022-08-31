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
      "x", 2
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
    mtcars, "means",
    .fn = mean
  )

  for (vn in names(dat2)) {
    expect_true(has_annotation(dat2[[vn]], "means"))
    expect_identical(annotation(dat2[[vn]], "means"), mean(dat2[[vn]]))
  }

  # Demonstrates that variable names are successfully passed in
  dat3 <- mutate_annotation_across(
    mtcars, "title",
    .fn = function(x, nx) nx, .with_names = TRUE
  )

  for (vn in names(dat3)) {
    expect_identical(vn, annotation(dat3[[vn]], "title"))
  }
})

test_that("Super annotations work correctly", {
  opt_old <- options(blueprintr.use_improved_annotations = TRUE)
  on.exit(options(opt_old))

  dat <- mtcars[, c("mpg", "cyl")]
  meta <- data.frame(
    name = c("mpg", "cyl"),
    type = c("double", "double"),
    description = c("MPG", "Number of cylinders")
  )

  dat <- mutate_annotation(dat, "description", mpg = "Miles per gallon")

  expect_identical(
    get_attr(dat$mpg, "bpr.super.description"),
    "Miles per gallon"
  )

  expect_identical(
    get_attr(dat$mpg, "bpr.description"),
    "Miles per gallon"
  )

  expect_null(get_attr(dat$cyl, "bpr.description"))

  # Simulating cleanup phase with mock data
  dat_cleanup <- annotate_variables(dat, list(annotate_overwrite = FALSE), meta)

  # Annotation happens anyway -- ignoring `annotate_overwrite`
  expect_identical(
    get_attr(dat_cleanup$cyl, "bpr.description"),
    "Number of cylinders"
  )

  # mutate_annotate() overrides metadata annotation
  expect_identical(
    get_attr(dat_cleanup$mpg, "bpr.description"),
    "Miles per gallon"
  )

  # Super annotation is removed
  expect_null(get_attr(dat_cleanup$mpg, "bpr.super.description"))
})
