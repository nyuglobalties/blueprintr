test_that("Metadata file dependency tables render correctly", {
  foo <- dplyr::tribble(
    ~name, ~type, ~description,
    "x", "integer", "An integer",
    "y", "character", "A string"
  )

  bar <- dplyr::tribble(
    ~name, ~type, ~description,
    "x", "character", "Not an integer",
    "z", "character", "Some other string"
  )

  baz <- dplyr::tribble(
    ~name, ~type,
    "x", "integer",
    "z", "character"
  )

  meta_dt_ok1 <- link_dependency_meta(baz, list(foo))
  meta_dt_ok2 <- link_dependency_meta(baz, list(bar))
  meta_dt_bad <- link_dependency_meta(baz, list(foo, bar))

  expect_equal(
    meta_dt_ok1[meta_dt_ok1$name == "x", ][["type"]],
    "integer"
  )

  expect_equal(
    meta_dt_ok2[meta_dt_ok2$name == "z", ][["description"]],
    "Some other string"
  )

  expect_false(
    meta_dt_ok2[meta_dt_ok2$name == "x", ][["type"]] ==
      meta_dt_ok2[meta_dt_ok2$name == "x", ][["deps_type"]]
  )

  expect_identical(
    meta_dt_bad[meta_dt_bad$name == "x", ][["deps_type"]],
    "integer|character"
  )

  expect_identical(
    meta_dt_bad[meta_dt_bad$name == "x", ][[".origin"]],
    "metafile"
  )
})

test_that("Annotation dependency tables render correctly", {
  dat <- dplyr::tibble(
    x = sample(0:3, 100, replace = TRUE),
    y = rnorm(100)
  )

  dat$x <- add_annotation(dat$x, "class", "ordinal")
  dat$y <- add_annotation(dat$y, "class", "continuous")

  meta_dt <- initial_metadata_dt(dat)
  linked <- link_annotation_meta(meta_dt, dat)

  expect_identical(
    linked[linked$name == "x", ][["class"]],
    "ordinal"
  )

  expect_identical(
    linked[linked$name == "x", ][[".origin"]],
    "annotations"
  )
})

test_that("Coalescing annotation vs. metafile deps works", {
  foo <- dplyr::tribble(
    ~name, ~type, ~description,
    "x", "integer", "An integer",
    "y", "character", "A string"
  )

  bar <- dplyr::tribble(
    ~name, ~type, ~description,
    "x", "character", "Not an integer",
    "z", "double", "Normally distributed thing"
  )

  baz <- dplyr::tribble(
    ~name, ~type,
    "x", "integer",
    "z", "double"
  )

  dat <- dplyr::tibble(
    x = sample(0:3, 100, replace = TRUE),
    y = rnorm(100)
  )

  dat$x <- add_annotation(dat$x, "class", "ordinal")
  dat$y <- add_annotation(dat$y, "class", "continuous")

  meta_dt <- initial_metadata_dt(dat)
  linked <- link_annotation_meta(meta_dt, dat)
  meta_dt_bad <- link_dependency_meta(baz, list(foo, bar))

  reconciled <- reconcile_dependencies(linked, meta_dt_bad)
})