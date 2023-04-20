`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

stopg <- function(x, .envir = parent.frame()) {
  stop0(
    glue::glue(
      glue::glue_collapse(x, sep = ""),
      .envir = .envir
    ),
    "\n"
  )
}

assert_string <- function(x) {
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
}

assert_maybe_string <- function(x) {
  stopifnot(is.null(x) || is.character(x))

  if (is.character(x)) assert_string(x)
}

`%if_empty_string%` <- function(x, y) {
  stopifnot(length(x) == 1)

  if (identical(x, "")) y else x
}

vapply_mold <- function(.type) {
  function(.x, .f, ...) {
    vapply(.x, .f, vector(.type, 1L), ...)
  }
}

vcapply <- vapply_mold("character")
vlapply <- vapply_mold("logical")
viapply <- vapply_mold("integer")
vdapply <- vapply_mold("double")

lapply2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    stats::setNames(out, names(.x))
  } else {
    stats::setNames(out, NULL)
  }
}

vapply2_mold <- function(type) {
  function(.x, .y, .f, ...) {
    as.vector(lapply2(.x, .y, .f, ...), type)
  }
}

vcapply2 <- vapply2_mold("character")
vlapply2 <- vapply2_mold("logical")
viapply2 <- vapply2_mold("integer")
vdapply2 <- vapply2_mold("double")

run_unit_tests <- function(path = here::here("tests")) {
  if (dir.exists(path)) {
    num_files <- length(list.files(path, pattern = "^test-.*\\.R"))

    if (num_files > 0) {
      testthat::test_dir(path)
    } else {
      message("No unit tests found in 'tests' folder")
    }
  }
}

#' Apply a function on an object without losing its attributes
#'
#' @param x An object
#' @param f A function
#' @param ... Any extra arguments for the function
#' @return The transformed object with its attributes intact
attr_safe <- function(x, f, ...) {
  attribs <- attributes(x)
  x <- f(x, ...)
  attributes(x) <- attribs
  x
}

#' Attribute-safe type coercion to character
#'
#' Converts objects of one type to character without
#' losing the object's attributes
#' @param x The object
#' @return The converted object, attributes intact
as_chr <- function(x) {
  attr_safe(x, as.character)
}

#' Attribute-safe type coercion to numeric
#'
#' Converts objects of one type to numeric without
#' losing the object's attributes
#' @param x The object
#' @return The converted object, attributes intact
as_num <- function(x) {
  attr_safe(x, as.numeric)
}

#' Attribute-safe type coercion to integer
#'
#' Converts objects of one type to integer without
#' losing the object's attributes
#' @param x The object
#' @return The converted object, attributes intact
as_int <- function(x) {
  attr_safe(x, as.integer)
}

#' Compute the mode without dropping mode collisions
#'
#' Rather than taking the first value from a multiple-valued mode from a collection, combine them together separated by | (for ease of pattern matching).
#' @param x A collection (generally a vector)
#' @return A length-1 character vector with all possible values of the mode of the collection
string_mode <- function(x) {
  mode <- paste0(names(which(table(x) == max(table(x), na.rm = TRUE))), collapse = "|")

  if (is.null(mode) || mode == "") mode <- NA_character_

  mode
}

full_flatten <- function(x, drop_pattern = NULL, restrict_pattern = NULL) {
  all_values <- as.character(as.vector(as.matrix(x)))

  if (!is.null(drop_pattern)) {
    all_values <- all_values[!grepl(drop_pattern, all_values)]
  }

  if (!is.null(restrict_pattern)) {
    all_values <- all_values[grepl(restrict_pattern, all_values)]
  }

  all_values
}

string_mode_sd <- function(x, drop_pattern = NULL, restrict_pattern = NULL) {
  all_values <- full_flatten(x, drop_pattern = drop_pattern, restrict_pattern = restrict_pattern)

  string_mode(all_values)
}

unique_val <- function(x) {
  ux <- unique(x)

  if (length(ux[!is.na(ux)]) > 0L) {
    ux[!is.na(ux)]
  } else {
    ux[is.na(ux)]
  }
}

unique_val_sd <- function(x, drop_pattern = NULL, restrict_pattern = NULL) {
  all_values <- full_flatten(x, drop_pattern = drop_pattern, restrict_pattern = restrict_pattern)

  vals <- unique_val(all_values)

  if (!all(is.na(vals))) {
    vals <- paste0(vals, collapse = "|")
  }

  vals
}

is_intlike <- function(x) {
  UseMethod("is_intlike", x)
}

is_intlike.default <- function(x) {
  FALSE
}

is_intlike.integer <- function(x) {
  TRUE
}

is_intlike.numeric <- function(x) {
  zeros <- rep(0, length(x[!is.na(x)]))

  isTRUE(all.equal(zeros, x[!is.na(x)] %% 1))
}

is_intlike.character <- function(x) {
  all(grepl("^\\d+$", x), na.rm = TRUE)
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
    stop0("All attribs must have names")
  }

  for (d in names(dots)) {
    obj <- set_attr(obj, d, dots[[d]])
  }

  obj
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

cat_line <- function(...) {
  cat(..., "\n", sep = "")
}

pattern_select <- function(patterns, pool) {
  res <- lapply(patterns, \(x) grep(x, pool, value = TRUE))

  unlist(res)
}
