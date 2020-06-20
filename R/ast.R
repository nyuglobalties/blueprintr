extract_ast <- function(expr) {
  if (identical(expr, bquote())) {
    return(expr)
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

find_ast_if <- function(ast, .p, recurse = TRUE) {
  if (!is_ast(ast)) {
    if (isTRUE(.p(ast))) {
      return(extract_ast(ast))
    } else {
      return(list())
    }
  } else {
    out <- list()

    if (isTRUE(.p(ast))) {
      out <- list(ast)
    } else {
      if (isTRUE(recurse)) {
        out <- lapply(ast$args, find_ast_if, .p)
      }
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

  if (is_namespaced_ast(ast)) {
    return(call2(ast$head, !!!collapsed_args, .ns = ast$ns))
  }

  if (is_qualified_ast(ast)) {
    browser()
  }

  call2(ast$head, !!!collapsed_args)
}

collapse_qualified_ast <- function(ast) {
  collapsed_args <- lapply(ast$args, collapse_ast)

  if (!is.null(ast$ns)) {
    calling_cmd <- expr((!!ast$qual_sym)(!!as.name(ast$ns), !!as.name(ast$head)))

    expr((!!calling_cmd)(!!!collapsed_args))
  } else {

  }
}

ast <- function(call) {
  if (identical(call_name(call), "function")) {
    return(function_ast(call)) 
  }

  if (is_qualified_call(call)) {
    return(qualified_ast(call))
  }

  structure(
    list(
      head = call_name(call),
      args = as.list(call)[-1]
    ),
    class = "ast"
  )
}

qualified_ast <- function(call) {
  qual_sym <- qualifier(call)
  qual_head <- qualified_head(call)
  namespace <- if (is_namespaced_call(call)) {
    call_ns(call)
  } else {
    NULL
  }

  structure(
    list(
      head = call_name(call),
      qual_head = qual_head,
      qual_sym = qual_sym,
      ns = namespace,
      args = as.list(call)[-1]
    ),
    class = c("ast", "qualified_ast")
  )
}

is_language <- function(x) {
  identical(typeof(x), "language")
}

qualifier <- function(call) {
  stopifnot(is_language(call))

  node_caar(call)
}

qualified_head <- function(call) {
  stopifnot(is_language(call))

  extract_ast(node_car(node_cdar(call)))
}

is_qualified_call <- function(call) {
  if (!is_language(call)) {
    return(FALSE)
  }

  if (!is_language(node_car(call))) {
    return(FALSE)
  }

  call_cmd <- node_car(call)

  if (!is_symbol(node_cadr(node_cdr(call_cmd)))) {
    return(FALSE)
  }

  qual_sym <- qualifier(call)

  identical(qual_sym, quote(`::`)) ||
    identical(qual_sym, quote(`:::`)) ||
    identical(qual_sym, quote(`$`)) ||
    identical(qual_sym, quote(`@`))
}

is_namespaced_call <- function(call) {
  if (!is_qualified_call(call)) {
    return(FALSE)
  }

  qual_sym <- qualifier(call)

  identical(qual_sym, quote(`::`)) ||
    identical(qual_sym, quote(`:::`))
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

is_qualified_ast <- function(x) {
  inherits(x, "qualified_ast")
}

is_namespaced_ast <- function(x) {
  if (!inherits(x, "qualified_ast")) {
    return(FALSE)
  }

  !is.null(x$ns)
}
