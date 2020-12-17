context("chunk-ast")

test_that("Chunk comprehension works", {
  command <- bquote({
    .TARGET("chunk1") %>% 
      left_join(
        .TARGET("chunk2") %>% 
        select(site_id, tent),
        by = "site_id"
      ) %>% 
      mutate(still_tent = tent_last == tent)
  })

  answer <- bquote({
  chunk1 %>% 
    left_join(
      chunk2 %>% 
        select(site_id, tent),
        by = "site_id"
      ) %>% 
      mutate(still_tent = tent_last == tent)
  })

  extracted <- extract_ast(command)
  modifed <- modify_ast_if(extracted, is_target_ast, eval_ast)

  expect_equal(collapse_ast(modifed), answer)
})
