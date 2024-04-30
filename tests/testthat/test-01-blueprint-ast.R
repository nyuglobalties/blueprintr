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
  modified <- modify_ast_if(extracted, is_target_ast, eval_ast)

  expect_equal(collapse_ast(modified), answer)

  command_2 <- quote(
    .TARGET("dataset") %>%
      mutate(across(
        c_01:c_10,
        function(.x) ifelse(cur_data()$c_id == 1234567, NA, .x)
      ))
  )

  answer_2 <- quote(
    dataset %>%
      mutate(across(
        c_01:c_10,
        function(.x) ifelse(cur_data()$c_id == 1234567, NA, .x)
      ))
  )

  extracted_2 <- extract_ast(command_2)
  modified_2 <- modify_ast_if(extracted_2, is_target_ast, eval_ast)

  expect_equal(collapse_ast(modified_2), answer_2)

  command_3 <- quote(
    .TARGET("dataset") %>%
      mutate(across(
        c_01:c_10,
        ~ ifelse(cur_data()$c_id == 1234567, NA, .x)
      ))
  )

  answer_3 <- quote(
    dataset %>%
      mutate(across(
        c_01:c_10,
        ~ ifelse(cur_data()$c_id == 1234567, NA, .x)
      ))
  )

  extracted_3 <- extract_ast(command_3)
  modified_3 <- modify_ast_if(extracted_3, is_target_ast, eval_ast)

  expect_equal(collapse_ast(modified_3), answer_3)

  command_4 <- quote(
    .TARGET("dataset") %>%
      processing$process(param = TRUE) %>%
      mutate(
        var = processing$func(variable)
      )
  )

  answer_4 <- quote(
    dataset %>%
      processing$process(param = TRUE) %>%
      mutate(
        var = processing$func(variable)
      )
  )

  extracted_4 <- extract_ast(command_4)
  modified_4 <- modify_ast_if(extracted_4, is_target_ast, eval_ast)

  expect_equal(collapse_ast(modified_4), answer_4)
})
