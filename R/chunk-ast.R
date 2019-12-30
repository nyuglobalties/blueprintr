# For now, just an alias to as.name, but will allow for more
# structured drake target names with configuration
.CHUNK <- function(chunk_name, ...) {
    as.name(chunk_name)
}

is_chunk_ast <- function(ast) {
    if (!is_ast(ast)) {
        return(FALSE)
    }

    identical(ast$head, ".CHUNK")
}

eval_chunk <- function(ast, env = parent.frame()) {
    collapsed <- collapse_ast(ast)

    eval_tidy(collapsed, env = env)
}
