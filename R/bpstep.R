#' Define a step of blueprint assembly
#'
#' Each step in the blueprint assembly process is contained in a wrapper
#' 'bpstep' object. `bpstep()` is the abstract class. `drake_bpstep()` and
#' `targets_bpstep()` are the implementations used within the `bpstep_*()`
#' methods.
#'
#' @param step The name of the step
#' @param bp A 'blueprint' object to create the assembled step
#' @param payload The assembled object, either a 'drake_plan' or 'tar_target'
#' @param ... Any extra information. Currently unused.
#' @param .class A character string to subclass `bpstep()`
#'
#' @return A 'bpstep' object
#' @keywords internal
#'
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

bpstep_payload <- function(assembler, ...) {
  UseMethod("bpstep_payload", assembler)
}

#' @export
bpstep_payload.drake_assembler <- function(
  assembler,
  target_name,
  target_command,
  ...
) {
  dots <- rlang::dots_list(...)

  if (length(dots) > 0) {
    quoted_target <- call2(
      "target",
      command = target_command,
      !!!dots,
      .ns = "drake"
    )
  } else {
    quoted_target <- target_command
  }

  argslist <- list(quoted_target)
  names(argslist) <- target_name

  do.call(drake::drake_plan, argslist)
}

#' @export
bpstep_payload.targets_assembler <- function(
  assembler,
  target_name,
  target_command,
  ...
) {
  targets::tar_target_raw(
    name = target_name,
    command = target_command,
    ...
  )
}
