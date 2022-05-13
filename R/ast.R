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
  } else {
    switch(
      head_sym_chr(.call),
      "~" = formula_ast(.call),
      "function" = function_ast(.call),
      structure(
        list(
          head = call_name(.call),
          args = as.list(.call)[-1]
        ),
        class = "ast"
      )
    )
  }
}

head_is_symbol <- function(.call) {
  is_symbol(node_car(.call))
}

head_sym_chr <- function(.call) {
  stopifnot(head_is_symbol(.call))

  as.character(node_car(.call))
}

qualified_ast <- function(.call) {
  qual_sym <- qualifier(.call)
  qual_head <- qualified_head(.call)

  if (is_namespaced_call(.call)) {
    namespace <- call_ns(.call)
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
  out <- call_name(.call)

  if (!is.null(out)) {
    return(out)
  }

  out <- node_cdar(.call)[[2]]

  if (!is_symbol(out)) {
    bp_err("Cannot identify head symbol: {safe_deparse(.call)}")
  }

  as.character(out)
}

is_language <- function(x) {
  identical(typeof(x), "language")
}

is_leaf <- function(x) {
  is_syntactic_literal(x) || is_symbol(x)
}

qualifier <- function(.call) {
  stopifnot(is_language(.call))

  node_caar(.call)
}

qualified_head <- function(.call) {
  stopifnot(is_language(.call))

  extract_ast(node_car(node_cdar(.call)))
}

is_qualified_call <- function(.call) {
  if (!is_language(.call)) {
    return(FALSE)
  }

  if (!is_language(node_car(.call))) {
    return(FALSE)
  }

  call_cmd <- node_car(.call)

  if (!is_symbol(node_cadr(node_cdr(call_cmd)))) {
    return(FALSE)
  }

  qual_sym <- qualifier(.call)

  identical(qual_sym, quote(`::`)) ||
    identical(qual_sym, quote(`:::`)) ||
    identical(qual_sym, quote(`$`)) ||
    identical(qual_sym, quote(`@`))
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
      head = call_name(.call),
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

  vlapply(ex$fargs, function(x) !(is_symbol(x) && identical(as.character(x), "")))
}
