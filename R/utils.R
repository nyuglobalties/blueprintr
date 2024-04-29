`%||%` <- function(x, y) if (is.null(x)) y else x

`%if_missing_str%` <- function(x, y) {
  stopifnot(length(x) == 1)
  stopifnot(is.character(x))

  if (is_missing(x)) y else x
}

is_missing <- function(x) {
  UseMethod("is_missing", x)
}

#' @export
is_missing.default <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }

  all(is.na(x))
}

#' @export
is_missing.character <- function(x) {
  all(is.na(x) | x == "")
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
}

vapply_mold <- function(type) {
  function(.x, .f, ...) {
    vapply(.x, .f, type, ...)
  }
}

viapply <- vapply_mold(integer(1L))
vcapply <- vapply_mold(character(1L))
vlapply <- vapply_mold(logical(1L))

lapply2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}

collapse_message_list <- function(x, and = TRUE) {
  x <- paste0("'", x, "'")

  if (length(x) > 1 && isTRUE(and)) {
    x[length(x)] <- paste("and", x[length(x)])
  }

  if (length(x) > 2) {
    paste0(x, collapse = ", ")
  } else {
    paste0(x, collapse = " ")
  }
}

safe_deparse <- function(x, collapse = "\n", backtick = TRUE, trim = FALSE, ...) {
  out <- deparse(x, backtick = backtick, ...)

  if (isTRUE(trim)) {
    out <- trimws(out)
  }

  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }

  out
}

cat_line <- function(x = NULL, indent = 0, .envir = parent.frame()) {
  ws <- rep("  ", indent)

  cat(ws, glue::glue(glue::glue_collapse(x), .envir = .envir), "\n", sep = "")
}

ui_value <- function(x) {
  paste0("'", x, "'")
}

ui_quote <- function(x) {
  paste0("`", x, "`")
}

bp_err <- function(x, ..., .envir = parent.frame()) {
  msg <- glue::glue(glue::glue_collapse(x), .envir = .envir)

  rlang::abort(class = "bp_error", message = msg, ...)
}

bp_warn <- function(x, .envir = parent.frame()) {
  msg <- glue::glue(glue::glue_collapse(x), .envir = .envir)

  rlang::warn(.subclass = "bp_warning", message = msg)
}

bp_msg <- function(x, .envir = parent.frame()) {
  msg <- glue::glue(glue::glue_collapse(x), .envir = .envir)

  message(msg)
}

bp_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- safe_deparse(substitute(x))
    msg <- glue::glue("Assertion {ui_quote(deparsed)} not met")
  } else {
    msg <- glue::glue(glue::glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    bp_err(msg)
  }

  invisible()
}

bp_path <- function(...) {
  system.file(..., package = "blueprintr")
}

string_to_coding <- function(x) {
  bp_assert(is.character(x) || is.logical(x))

  if (!requireNamespace("rcoder", quietly = TRUE)) {
    bp_err("`rcoder` is not installed. Cannot evaluate coding string.")
  }

  lapply(x, string_to_coding_single)
}

string_to_coding_single <- function(x) {
  bp_assert(length(x) == 1)

  if (x == "" || is.na(x)) {
    return(rcoder::empty_coding())
  }

  tryCatch(
    rcoder::eval_coding(rlang::parse_expr(x)),
    error = function(e) {
      bp_err("Could not evaluate coding: '{x}'")
    }
  )
}

flatten <- function(x) {
  stopifnot(is.list(x))

  total_length <- sum(viapply(x, length))
  flattened <- vector("list", total_length)
  k <- 1

  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      flattened[[k]] <- x[[i]][[j]]
      k <- k + 1
    }
  }

  flattened
}

has_attr <- function(obj, key) {
  !is.null(get_attr(obj, key))
}

get_attr <- function(obj, key) {
  attr(obj, key, exact = TRUE)
}

set_attr <- function(obj, key, value) {
  attr(obj, key) <- value
  obj
}

set_attrs <- function(obj, ...) {
  dots <- rlang::dots_list(...)

  if (is.null(names(dots)) || any(names(dots) == "")) {
    bp_err("All attribs must have names")
  }

  for (d in names(dots)) {
    obj <- set_attr(obj, d, dots[[d]])
  }

  obj
}

blueprintr_attrs <- function(dat) {
  var_attrs <- lapply(dat, attributes)
  lapply(var_attrs, function(attrs) attrs[grepl("^(bpr\\.|\\.uuid)", names(attrs))])
}

preserve_blueprintr_attrs <- function(dat, f, ..., .f_of_dat = TRUE) {
  bpr_var_attrs <- blueprintr_attrs(dat)

  if (isTRUE(.f_of_dat)) {
    dat <- f(dat, ...)
  } else {
    dat <- f(...)
  }

  for (nvar in names(bpr_var_attrs)) {
    if (!is.null(bpr_var_attrs[[nvar]])) {
      dat[[nvar]] <- set_attrs(dat[[nvar]], !!!bpr_var_attrs[[nvar]])
    }
  }

  dat
}

attr_safe <- function(.x, .f, ..., .p = NULL) {
  attribs <- attributes(.x)

  if (!is.null(.p)) {
    attribs <- attribs[.p(names(attribs))]
  }

  .x <- .f(.x, ...)
  set_attrs(.x, !!!attribs)
}

unique_val <- function(x) {
  ux <- unique(x)

  if (length(ux[!is.na(ux)]) > 0L) {
    ux[!is.na(ux)]
  } else {
    ux[is.na(ux)]
  }
}
