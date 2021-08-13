`%||%` <- function(x, y) if (is.null(x)) y else x

`%if_missing_str%` <- function(x, y) {
  stopifnot(length(x) == 1)
  stopifnot(is.character(x))

  if (is_missing(x)) y else x
}

is_missing <- function(x) {
  UseMethod("is_missing", x)
}

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

viapply <- function(.x, .f, ...) vapply(.x, .f, integer(1L), ...)
vcapply <- function(.x, .f, ...) vapply(.x, .f, character(1L), ...)
vlapply <- function(.x, .f, ...) vapply(.x, .f, logical(1L), ...)

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

  cat(ws, glue(glue_collapse(x), .envir = .envir), "\n", sep = "")
}

ui_value <- function(x) {
  paste0("'", x, "'")
}

ui_quote <- function(x) {
  paste0("`", x, "`")
}

bp_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::abort(.subclass = "bp_error", message = msg)
}

bp_warn <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::warn(.subclass = "bp_warning", message = msg)
}

bp_msg <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  message(msg)
}

bp_msg_verbose <- function(verbose, x, .envir = parent.frame()) {
  if (isTRUE(verbose)) {
    bp_msg(x, .envir = .envir)
  }
}

bp_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- safe_deparse(substitute(x))
    msg <- glue("Assertion {ui_quote(deparsed)} not met")
  } else {
    msg <- glue(glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    bp_err(msg)
  }

  invisible()
}

bp_path <- function(...) {
  system.file(..., package = "blueprintr")
}

assert_pkg <- function(pkg, version = NULL, install = "install.packages") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "package ", pkg, " not installed. Install with ",
      install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }

  if (is.null(version)) {
    return()
  }

  installed_version <- as.character(utils::packageVersion(pkg))
  is_too_old <- utils::compareVersion(installed_version, version) < 0

  if (is_too_old) {
    stop(
      "package ", pkg, " must be version ", version, " or greater. ",
      "Found version ", version, " installed.",
      "Update it with ", install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }

  invisible()
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

  rcoder::eval_coding(rlang::parse_expr(x))
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

get_attr <- function(obj, attrib) {
  attr(obj, attrib, exact = TRUE)
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

unique_val <- function(x) {
  ux <- unique(x)

  if (length(ux[!is.na(ux)]) > 0L) {
    ux[!is.na(ux)]
  } else {
    ux[is.na(ux)]
  }
}