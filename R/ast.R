extract_ast <- function(expr) {
    # Protect from weird bquote.. bug?
    if (identical(expr, bquote())) {
        abort("Cannot expand AST of empty expression.")
    }

    # Base cases
    if (is_syntactic_literal(expr) || is_symbol(expr)) {
        return(expr)
    }

    # Handle translation of AST
    if (is_ast(expr)) {
        if (is_function_ast(expr)) {
            expr$fargs <- lapply(expr$fargs, extract_ast)
        }

        expr$args <- lapply(expr$args, extract_ast)
        return(expr)
    } else if (!is.language(expr)) {
        abort("extract_ast only handles expressions and ast objects.")
    }

    extract_ast(ast(expr))
}

modify_ast_if <- function(ast, .p, .f, ..., recurse = TRUE) {
    if (!is_ast(ast)) {
        if (isTRUE(.p(ast))) {
            return(.f(ast, ...))
        } else {
            return(ast)
        }
    } else {
        out <- ast

        if (isTRUE(.p(ast))) {
            out <- .f(ast, ...)
        }

        if (isTRUE(recurse) && is_ast(out)) {
            if (is_function_ast(out)) {
                out$fargs <- lapply(out$fargs, modify_ast_if, .p, .f, ...)
            }

            out$args <- lapply(out$args, modify_ast_if, .p, .f, ...)
        }

        out
    }
}

collapse_ast <- function(ast) {
    if (!is_ast(ast)) {
        return(ast)
    }

    if (is_function_ast(ast)) {
        fargs <- lapply(ast$fargs, collapse_ast)
        body <- collapse_ast(ast$args)
        
        return(call(ast$head, as.pairlist(fargs), body))
    }

    collapsed_args <- lapply(ast$args, collapse_ast)

    if (!is.null(ast$ns)) {
        calling_cmd <- expr(`::`(!!as.name(ast$ns), !!as.name(ast$head)))
        expr((!!calling_cmd)(!!!collapsed_args))
    } else {
        call2(ast$head, !!!collapsed_args)
    }
}

ast <- function(call) {
    if (identical(call_name(call), "function")) {
        return(function_ast(call)) 
    }

    structure(
        list(
            head = call_name(call),
            ns   = call_ns(call),
            args = as.list(call)[-1]
        ),
        class = "ast"
    )
}

function_ast <- function(call) {
    call_list <- as.list(call)

    structure(
        list(
            head = call_name(call),
            fargs = as.list(call_list[[2]]),
            args = ast(call_list[[3]])
        ),
        class = c("ast", "function_ast")
    )
}

is_ast <- function(x) {
    inherits(x, "ast")
}

is_function_ast <- function(x) {
    inherits(x, "function_ast")
}
