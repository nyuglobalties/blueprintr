#' Send multiple objects into a function, preserving attributes
#'
#' Unlike `attr_safe`, this assumes that multiple inputs
#' are going into a function. The output preserves the
#' attributes as a union, similar to the behavior of
#' [dplyr::bind_rows]
#'
#' @param f A function that accept multiple inputs. `f` must
#'   have no keyword arguments!
#' @param ... Arguments forwarded to `f`
#' @return Output of `f(...)` with unioned attributes
attr_safe_variadic <- function(f, ...) {
  dots <- rlang::dots_list(...)

  all_attrs <- lapply(dots, attributes)
  attr_keys <- unique(unlist(lapply(all_attrs, names)))
  names(attr_keys) <- attr_keys

  new_attrs <- lapply(attr_keys, function(key) {
    attrs_for_key <- unlist(lapply(
      all_attrs,
      function(x) as.character(x[[key]])
    ))

    paste0(unique(attrs_for_key), collapse = "|")
  })

  out <- f(...)
  attributes(out) <- new_attrs
  out
}

attr_safe_coalesce <- function(...) {
  attr_safe_variadic(dplyr::coalesce, ...)
}

test_that("Basic variable lineage works correctly", {
  set.seed(10019)

  parent_bp <- blueprint(
    "parent_table",
    command = {
      mtcars
    }
  )

  child_bp <- blueprint(
    "child_table",
    command = {
      .TARGET("parent_bp") %>%
        dplyr::mutate(new_var = rnorm(dplyr::n()))
    }
  )

  parent_bp_dat <- mtcars
  parent_bp_dat <- add_variable_uuids(parent_bp_dat)

  child_bp_dat <- parent_bp_dat %>% dplyr::mutate(new_var = rnorm(dplyr::n()))
  child_bp_dat <- add_variable_uuids(child_bp_dat)

  child_dat_table <- vl_dat_table(child_bp_dat, "child_table")
  child_deps <- list(parent_table = parent_bp_dat)
  child_dep_table <- vl_dep_table_from_deps(child_deps)

  # UUIDs are inherited correctly
  expect_setequal(
    child_dep_table$uuid,
    child_dat_table[child_dat_table$varname != "new_var", "uuid", drop = TRUE]
  )

  edges_from_parent_table <- vl_edge_table(child_dat_table, child_deps)$edges

  # Edges are captured correctly and have correct flow
  expect_setequal(
    edges_from_parent_table$uuid,
    child_dat_table[child_dat_table$varname != "new_var", "uuid", drop = TRUE]
  )

  expect_true(all(edges_from_parent_table$from %in% child_dep_table$id))
  expect_true(all(edges_from_parent_table$to %in% child_dat_table$id))

  # Expect an inner join -- no missingness
  expect_true(!any(is.na(edges_from_parent_table$from)))
  expect_true(!any(is.na(edges_from_parent_table$to)))
})

test_that("Parent variables are treated correctly", {
  parent_bp <- blueprint(
    "parent_table",
    command = {
      mtcars
    }
  )

  child_bp <- blueprint(
    "child_table",
    command = {
      .TARGET("parent_bp") %>%
        dplyr::mutate(new_var = attr_safe_coalesce(mpg, cyl))
    }
  )

  parent_bp_dat <- mtcars
  parent_bp_dat <- add_variable_uuids(parent_bp_dat)

  # Testing using an "attribute-safe" form of dplyr::coalesce()
  # that keeps a union of all attributes
  child_bp_dat <- parent_bp_dat %>% dplyr::mutate(new_var = attr_safe_coalesce(mpg, cyl))
  child_bp_dat <- add_variable_uuids(child_bp_dat)

  child_dat_table <- vl_dat_table(child_bp_dat, "child_table")
  child_deps <- list(parent_table = parent_bp_dat)
  child_dep_table <- vl_dep_table_from_deps(child_deps)

  parent_vars <- vl_parent_vars(child_dat_table, child_dep_table)

  expect_true(all(parent_vars$from %in% child_dat_table$id))
})

test_that("Lineage with deep ancestry works", {
  root_bp <- blueprint(
    "root_table",
    command = {
      mtcars
    }
  )

  part1_bp <- blueprint(
    "part1_table",
    command =
      .TARGET("root_table") %>%
        dplyr::select(mpg, cyl)
  )

  part2_bp <- blueprint(
    "part2_table",
    command =
      .TARGET("root_table") %>%
        dplyr::select(disp, hp)
  )

  combined_bp <- blueprint(
    "combined_table",
    command = cbind(.TARGET("part1_table"), .TARGET("part2_table"))
  )

  root_bp_dat <- mtcars
  root_bp_dat <- add_variable_uuids(root_bp_dat)

  part1_bp_dat <- dplyr::select(root_bp_dat, mpg, cyl)
  part1_bp_dat <- add_variable_uuids(part1_bp_dat)

  part2_bp_dat <- dplyr::select(root_bp_dat, disp, hp)
  part2_bp_dat <- add_variable_uuids(part2_bp_dat)

  combined_bp_dat <- cbind(part1_bp_dat, part2_bp_dat)
  combined_bp_dat <- add_variable_uuids(combined_bp_dat)

  # Assert that the same pool of UUIDs is being used -- necessary for
  # checking that all edges are computed correctly
  expect_true(all(table_uuid_attrs(part1_bp_dat) %in% table_uuid_attrs(root_bp_dat)))
  expect_true(all(table_uuid_attrs(part2_bp_dat) %in% table_uuid_attrs(root_bp_dat)))
  expect_true(all(table_uuid_attrs(combined_bp_dat) %in% table_uuid_attrs(root_bp_dat)))

  parts_deps <- list(root_table = root_bp_dat)
  combined_deps <- list(part1_table = part1_bp_dat, part2_table = part2_bp_dat)

  root_dep_tables <- blueprint_variable_dep_tables(root_bp, root_bp_dat)
  part1_dep_tables <- blueprint_variable_dep_tables(part1_bp, part1_bp_dat, deps = parts_deps)
  part2_dep_tables <- blueprint_variable_dep_tables(part2_bp, part2_bp_dat, deps = parts_deps)
  combined_dep_tables <- blueprint_variable_dep_tables(combined_bp, combined_bp_dat, deps = combined_deps)

  expect_true(all(part1_dep_tables$edges$from %in% root_dep_tables$node$id))
  expect_true(all(part2_dep_tables$edges$from %in% root_dep_tables$node$id))

  expect_true(all(
    combined_dep_tables$edges$from %in%
      union(part1_dep_tables$node$id, part2_dep_tables$node$id)
  ))

  all_nodes <- rbind(
    root_dep_tables$node,
    part1_dep_tables$node,
    part2_dep_tables$node,
    combined_dep_tables$node
  )

  all_edges <- rbind(
    root_dep_tables$edges,
    part1_dep_tables$edges,
    part2_dep_tables$edges,
    combined_dep_tables$edges
  )

  expect_true(length(unique(all_nodes$id)) == nrow(all_nodes))
  expect_setequal(all_nodes$database_type, "blueprint")
})

test_that("Cleanup doesn't mess up attributes", {
  test_bp <- blueprint(
    "test_table",
    command = mtcars[, c("mpg", "vs")]
  )

  test_meta <- data.frame(
    name = c("mpg", "vs"),
    type = c("double", "double"),
    description = c("Miles per gallon", "Is engine V-shaped?"),
    coding = c(NA_character_, "coding(code('Yes', 1), code('No', 0))")
  )

  test_dat <- mtcars[, c("mpg", "vs")]

  # Add UUIDs to simulate drawing from parent data
  test_dat <- add_variable_uuids(test_dat)

  out <- label_columns(test_dat, test_bp, test_meta)

  expect_identical(uuid_attr(out$mpg), uuid_attr(test_dat$mpg))
  expect_identical(uuid_attr(out$vs), uuid_attr(test_dat$vs))
})

test_that("Variable lineage works with sources", {
  source_dat <- mark_source(mtcars)
  # Error only raised if UUIDs are NULL, so expect silent
  expect_silent(table_uuid_attrs(source_dat))

  test_bp <- blueprint(
    "test_table",
    command = .SOURCE("source_dat")[, 1:3]
  )

  test_bp_dat <- source_dat[, 1:3]
  test_deps <- list(source_dat = source_dat)

  expect_identical(blueprint_source_deps(test_bp), "source_dat")
  test_dep_tables <- blueprint_variable_dep_tables(test_bp, test_bp_dat, deps = test_deps)

  expect_setequal(test_dep_tables$deps$database, "source_dat")
  expect_setequal(test_dep_tables$deps$database_type, "source")
})

test_that("Table lineage works with sources", {
  # mark_source() not needed for table lineage
  test_bp <- blueprint(
    "test_table",
    command = .SOURCE("source_dat")[, 1:3]
  )

  test_dep_tables <- blueprint_dependency_tables(test_bp)
  expect_setequal(test_dep_tables$sources$type, "source")
  expect_setequal(test_dep_tables$sources$name, "source_dat")
})

test_that("Requested igraphs are rendered correctly", {
  skip_if_not_installed("igraph")

  source_dat <- mark_source(mtcars)
  test_bp <- blueprint(
    "test_table",
    command = .SOURCE("source_dat")[, 1:3]
  )

  test_bp_dat <- source_dat[, 1:3]
  test_deps <- list(source_dat = source_dat)

  g1 <- get_table_linage_igraph(list(test_bp))
  expect_s3_class(g1, "igraph")
  expect_setequal(names(igraph::V(g1)), c("source_dat", "test_table"))

  g2 <- get_variable_lineage_igraph(
    list(test_bp),
    dats = list(test_bp_dat),
    deps = list(test_deps)
  )
  expect_s3_class(g2, "igraph")
  expect_setequal(
    igraph::V(g2)$varname,
    names(source_dat)
  )
})

test_that("Filtering variable lineage", {
  root_bp <- blueprint(
    "root_table",
    command = {
      mtcars
    }
  )

  part1_bp <- blueprint(
    "part1_table",
    command =
      .TARGET("root_table") %>%
        dplyr::select(mpg, cyl)
  )

  part2_bp <- blueprint(
    "part2_table",
    command =
      .TARGET("root_table") %>%
        dplyr::select(disp, hp)
  )

  combined_bp <- blueprint(
    "combined_table",
    command = cbind(.TARGET("part1_table"), .TARGET("part2_table"))
  )

  root_bp_dat <- mtcars
  root_bp_dat <- add_variable_uuids(root_bp_dat)

  part1_bp_dat <- dplyr::select(root_bp_dat, mpg, cyl)
  part1_bp_dat <- add_variable_uuids(part1_bp_dat)

  part2_bp_dat <- dplyr::select(root_bp_dat, disp, hp)
  part2_bp_dat <- add_variable_uuids(part2_bp_dat)

  combined_bp_dat <- cbind(part1_bp_dat, part2_bp_dat)
  combined_bp_dat <- add_variable_uuids(combined_bp_dat)

  parts_deps <- list(root_table = root_bp_dat)
  combined_deps <- list(part1_table = part1_bp_dat, part2_table = part2_bp_dat)

  g <- get_variable_lineage_igraph(
    list(root_bp, part1_bp, part2_bp, combined_bp),
    dats = list(root_bp_dat, part1_bp_dat, part2_bp_dat, combined_bp_dat),
    deps = list(NULL, parts_deps, parts_deps, combined_deps)
  )

  g_mpg <- filter_variable_lineage(g, variables = "mpg")

  expect_setequal(igraph::V(g_mpg)$varname, "mpg")

  g_mpg_part <- filter_variable_lineage(g, variables = "mpg", tables = "part", mode = "in")
  expect_setequal(igraph::V(g_mpg)$varname, "mpg")

  # Using 'in' should only include ancestors, not children
  expect_setequal(igraph::V(g_mpg_part)$database, c("part1_table", "root_table"))
})
