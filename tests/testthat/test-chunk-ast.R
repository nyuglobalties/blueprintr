context("chunk-ast")

test_that("Chunk comprehension works", {
    command <- bquote({
        .CHUNK("chunk1") %>% 
            left_join(
                .CHUNK("chunk2") %>% 
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
    modifed <- modify_ast_if(extracted, is_chunk_ast, eval_chunk)

    expect_equal(collapse_ast(modifed), answer)
})
