ast <- function(.call) {
  if (is_leaf(.call)) {
    return(.call)
  }

  if (!head_is_symbol(.call)) {
    if (is_qualified_call(.call)) {
      qualified_ast(.call)
    } else {
      bp_err("Unknown call structure: {safe_deparse(.call)}")
    }
  } else if (is_qualified_expr(.call)) {
    # For pure qualified expressions, not qualified call weirdness
    structure(
      list(
        head = as.character(qualifier(.call)),
        args = as.list(.call)[-1]
      ),
      class = "ast"
    )
  } else {
    switch(head_sym_chr(.call),
      "~" = formula_ast(.call),
      "function" = function_ast(.call),
      structure(
        list(
          head = rlang::call_name(.call),
          args = as.list(.call)[-1]
        ),
        class = "ast"
      )
    )
  }
}

head_is_symbol <- function(.call) {
  rlang::is_symbol(rlang::node_car(.call))
}

head_sym_chr <- function(.call) {
  stopifnot(head_is_symbol(.call))

  as.character(rlang::node_car(.call))
}

qualified_ast <- function(.call) {
  qual_sym <- qualifier(.call)
  qual_head <- qualified_head(.call)

  if (is_namespaced_call(.call)) {
    namespace <- rlang::call_ns(.call)
  } else {
    namespace <- NULL
  }

  structure(
    list(
      head = call_name_(.call),
      qual_head = qual_head,
      qual_sym = qual_sym,
      ns = namespace,
      args = as.list(.call)[-1]
    ),
    class = c("qualified_ast", "ast")
  )
}

call_name_ <- function(.call) {
  out <- rlang::call_name(.call)

  if (!is.null(out)) {
    return(out)
  }

  out <- rlang::node_cdar(.call)[[2]]

  if (!rlang::is_symbol(out)) {
    bp_err("Cannot identify head symbol: {safe_deparse(.call)}")
  }

  as.character(out)
}

is_language <- function(x) {
  identical(typeof(x), "language")
}

is_leaf <- function(x) {
  rlang::is_syntactic_literal(x) || rlang::is_symbol(x)
}

qualifier <- function(.call) {
  stopifnot(is_language(.call))

  if (is_qualified_call(.call)) {
    return(qualifier(rlang::node_car(.call)))
  }

  if (is_qualified_expr(.call)) {
    rlang::node_car(.call)
  } else {
    NULL
  }
}

qualified_head <- function(.call) {
  stopifnot(is_language(.call))

  extract_ast(rlang::node_car(rlang::node_cdar(.call)))
}

is_qualified_expr <- function(.call) {
  if (!is_language(.call)) {
    return(FALSE)
  }

  if (!rlang::is_symbol(rlang::node_car(.call))) {
    return(FALSE)
  }

  if (length(rlang::node_cdr(.call)) != 2) {
    return(FALSE)
  }

  head_sym <- rlang::node_car(.call)

  identical(head_sym, quote(`::`)) ||
    identical(head_sym, quote(`:::`)) ||
    identical(head_sym, quote(`$`)) ||
    identical(head_sym, quote(`@`))
}

is_qualified_call <- function(.call) {
  if (!is_language(.call)) {
    return(FALSE)
  }

  if (!is_language(rlang::node_car(.call))) {
    return(FALSE)
  }

  is_qualified_expr(rlang::node_car(.call))
}

is_namespaced_call <- function(.call) {
  if (!is_qualified_call(.call)) {
    return(FALSE)
  }

  qual_sym <- qualifier(.call)

  identical(qual_sym, quote(`::`)) ||
    identical(qual_sym, quote(`:::`))
}

formula_ast <- function(.call) {
  call_list <- as.list(.call)

  structure(
    list(
      head = "~",
      args = call_list[-1]
    ),
    class = c("formula_ast", "ast")
  )
}

function_ast <- function(.call) {
  call_list <- as.list(.call)

  structure(
    list(
      head = rlang::call_name(.call),
      fargs = as.list(call_list[[2]]),
      args = call_list[[3]]
    ),
    class = c("function_ast", "ast")
  )
}

is_ast <- function(x) {
  inherits(x, "ast")
}

is_function_ast <- function(x) {
  inherits(x, "formula_ast")
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

mutable_fargs <- function(ex) {
  stopifnot(is_function_ast(ex))

  vlapply(ex$fargs, function(x) !(rlang::is_symbol(x) && identical(as.character(x), "")))
}
