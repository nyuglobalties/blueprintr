blueprint_test_result <- function(...) {
  structure(
    list(...),
    class = c("bp_test_result", "list")
  )
}

accept <- function(x, ...) {
  UseMethod("accept")
}

accept.bp_test_result <- function(x) {
  structure(
    x,
    pass = TRUE
  )
}

reject <- function(x, ...) {
  UseMethod("reject")
}

reject.bp_test_result <- function(x) {
  structure(
    x,
    pass = FALSE
  )
}

print.bp_test_result <- function(x, ...) {
  cat("<blueprint test result>\n")

  if (length(x) == 0) {
    cat("Test has no messages.\n")
  } else {
    cat(glue("Test has {length(x)} message{if (length(x) != 1) 's' else ''}:"), "\n", sep = "")

    for (msg in x) {
      cat(msg, "\n", sep = "")
    }
  }

  invisible()
}

results_dt <- function(test_name, x) {
  stopifnot(inherits(x, "bp_test_result"))

  if (length(x) == 0) {
    all_messages <- NA_character_
  } else {
    all_messages <- unlist(x)
  }

  data.table(test = test_name, pass = attr(x, "pass") %||% FALSE, messages = all_messages)
}

add_test_message <- function(test_result, ...) {
  stopifnot(inherits(test_result, "bp_test_result"))

  dots <- unlist(dots_list(...))
  message <- glue_collapse(dots, "\n")

  test_result[[length(test_result) + 1]] <- message
  test_result
}
