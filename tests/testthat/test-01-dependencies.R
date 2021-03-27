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
    ~name, ~type, ~description, ~extra,
    "x", "integer", "An integer", "not be seen",
    "y", "character", "A string", NA
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
  dat$x <- add_annotation(dat$x, "extra", "will be seen")
  dat$y <- add_annotation(dat$y, "class", "continuous")

  meta_dt <- initial_metadata_dt(dat)
  linked <- link_annotation_meta(meta_dt, dat)
  meta_dt_bad <- link_dependency_meta(baz, list(foo, bar))

  reconciled <- reconcile_dependencies(linked, meta_dt_bad)

  # Demontrates that the actual variables found in the
  # dataset are preserved
  expect_true(
    setequal(reconciled$name, c("x", "y"))
  )

  # Demontrates that the actual type information of
  # the underlying is preserved
  expect_identical(
    reconciled$type[[1]],
    "integer"
  )

  # Demonstrates proper ordering during coalescing
  expect_identical(
    reconciled$extra[[1]],
    "will be seen"
  )

  # Demonstrates the identification of type conflict
  # in metafile-based dependency linkage
  expect_identical(
    reconciled$deps_type[[1]],
    "integer|character"
  )

  # Demonstrates that fields not in metafile-type
  # dependencies are kept
  expect_identical(
    reconciled$class[[1]],
    "ordinal"
  )

  expect_identical(
    reconciled$class[[2]],
    "continuous"
  )
})