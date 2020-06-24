`%||%` <- function(x, y) if (is.null(x)) y else x

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

cat_line <- function(x = NULL) {
  cat(x, "\n", sep = "")
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
