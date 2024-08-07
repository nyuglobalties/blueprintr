test_that("Metadata file dependency tables render correctly", {
  foo <- tidytable::tribble(
    ~name, ~type, ~description,
    "x", "integer", "An integer",
    "y", "character", "A string"
  )

  bar <- tidytable::tribble(
    ~name, ~type, ~description,
    "x", "character", "Not an integer",
    "z", "character", "Some other string"
  )

  baz <- tidytable::tribble(
    ~name, ~type,
    "z", "character",
    "x", "integer"
  )

  meta_dt_ok1 <- link_dependency_meta(baz, list(foo))
  meta_dt_ok2 <- link_dependency_meta(baz, list(bar))
  meta_dt_bad <- link_dependency_meta(baz, list(foo, bar))

  # Order is preserved
  expect_identical(meta_dt_ok1$name, c("z", "x"))
  expect_identical(meta_dt_ok2$name, c("z", "x"))

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
  dat <- tidytable::tidytable(
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
  foo <- tidytable::tribble(
    ~name, ~type, ~description, ~extra,
    "y", "character", "A string", NA,
    "x", "integer", "An integer", "not be seen",
  )

  bar <- tidytable::tribble(
    ~name, ~type, ~description,
    "x", "character", "Not an integer",
    "z", "double", "Normally distributed thing"
  )

  baz <- tidytable::tribble(
    ~name, ~type,
    "x", "integer",
    "z", "double"
  )

  dat <- tidytable::tidytable(
    y = rnorm(100),
    x = sample(0:3, 100, replace = TRUE)
  )

  dat$x <- add_annotation(dat$x, "class", "ordinal")
  dat$x <- add_annotation(dat$x, "extra", "will be seen")
  dat$y <- add_annotation(dat$y, "class", "continuous")

  meta_dt <- initial_metadata_dt(dat)
  linked <- link_annotation_meta(meta_dt, dat)
  meta_dt_bad <- link_dependency_meta(baz, list(foo, bar))

  reconciled <- reconcile_dependencies(linked, meta_dt_bad)

  # Demonstrates that the actual variables found in the
  # dataset are preserved
  expect_true(
    setequal(reconciled$name, c("x", "y"))
  )

  # Demonstrates that the actual type information of
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

test_that("Metadata propagation corner cases are covered", {
  # 1. No dependencies being provided doesn't leave behind
  #    ".origin" column
  # 2. "description" field always exists
  mtcars_meta <- initial_metadata_dt(mtcars)
  meta_dt <- propagate_metadata(mtcars_meta, mtcars, list())

  expect_true(!".origin" %in% names(meta_dt))
  expect_true(all(
    c("name", "type", "description") %in% names(meta_dt)
  ))
})
