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
  expr6 <- quote(~.x)
  expr7 <- quote(function(.x) {
    .x + 1
  })
  expr8 <- quote(dplyr:::abort_glue(words))
  expr9 <- quote(stuff@hidden_function(words))
  expr10 <- quote(stuff$thing(words))
  expr11 <- quote(func(function(.x) ifelse(is.na(.x), 0, .x)))
  expr12 <- quote(df %>% module$func(x = stuff))
  expr13 <- quote(thing == TRUE ~ x)

  ast_ident <- function(e) collapse_ast(extract_ast(e))

  expect_equal(ast_ident(expr1), expr1)
  expect_equal(ast_ident(expr2), expr2)
  expect_equal(ast_ident(expr3), expr3)
  expect_equal(ast_ident(expr4), expr4)
  expect_equal(ast_ident(expr5), expr5)
  expect_equal(ast_ident(expr6), expr6)
  expect_equal(ast_ident(expr7), expr7)
  expect_equal(ast_ident(expr8), expr8)
  expect_equal(ast_ident(expr9), expr9)
  expect_equal(ast_ident(expr10), expr10)
  expect_equal(ast_ident(expr11), expr11)
  expect_equal(ast_ident(expr12), expr12)
  expect_equal(ast_ident(expr13), expr13)
})

test_that("Corner cases are covered", {
  some_obj <- list(x = 1, y = letters)
  expect_identical(collapse_ast(some_obj), some_obj)
})