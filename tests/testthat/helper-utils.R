expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

mock_drake_plan <- function(...) {
  dots <- match.call(expand.dots = FALSE)$...

  out <- data.frame(
    target = names(dots)
  )
  out$command <- dots

  structure(
    out,
    class = c("drake_plan", class(out))
  )
}

tar_make_local <- function(..., envir = parent.frame()) {
  targets::tar_make(..., callr_function = NULL, envir = envir)
}

tar_manifest_local <- function(..., envir = parent.frame()) {
  targets::tar_manifest(..., callr_function = NULL, envir = envir)
}
