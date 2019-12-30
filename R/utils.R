`%||%` <- function(x, y) if (is.null(x)) y else x

viapply <- function(.x, .f, integer, ...) vapply(.x, .f, integer(1L), ...)
vcapply <- function(.x, .f, character, ...) vapply(.x, .f, character(1L), ...)
vlapply <- function(.x, .f, logical, ...) vapply(.x, .f, logical(1L), ...)

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
