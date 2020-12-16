bpstep <- function(step, bp, payload, ..., .class = NULL) {
  stopifnot(is_blueprint(bp))
  stopifnot(is.character(step))

  structure(
    list(
      step = step,
      payload = payload,
      blueprint = bp,
      ...
    ),
    class = c(.class, "bpstep")
  )
}

#' @export
print.bpstep <- function(x, ...) {
  cat_line("<blueprint assembly step>")
  cat_line("name: {x$step}", indent = 1)
  cat_line("blueprint: '{x$blueprint$name}'", indent = 1)
  cat_line("class: '{class(x)[1]}'", indent = 1)
  cat_line()
  cat_line("payload:")
  print(x$payload)

  invisible(x)
}

drake_bpstep <- function(step, bp, payload, ...) {
  bpstep(
    step = step,
    bp = bp,
    payload = payload,
    ...,
    .class = "drake_bpstep"
  )
}

targets_bpstep <- function(step, bp, payload, ...) {
  bpstep(
    step = step,
    bp = bp,
    payload = payload,
    ...,
    .class = "targets_bpstep"
  )
}
