context("ast")

test_that("extract_ast and collapse_ast are inverses", {
  expr1 <- bquote(x + 1)
  expr2 <- bquote({
    x <- if (y < x^2) {
      y
    } else {
      x^2
    }

    x
  })
  expr3 <- bquote(codetools::showTree(expr1))

  expr4 <- quote(func(function(.x) .x, param = "test"))
  expr5 <- quote(func(function(.x, .p = "word") .x, param = "test"))

  ast_ident <- function(e) collapse_ast(extract_ast(e))

  expect_equal(ast_ident(expr1), expr1)
  expect_equal(ast_ident(expr2), expr2)
  expect_equal(ast_ident(expr3), expr3)
  expect_equal(ast_ident(expr4), expr4)
  expect_equal(ast_ident(expr5), expr5)
})

test_that("Corner cases are covered", {
  some_obj <- list(x = 1, y = letters)
  expect_identical(collapse_ast(some_obj), some_obj)
})
