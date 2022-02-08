#' Define a step of blueprint assembly
#'
#' Each step in the blueprint assembly process is contained in a wrapper
#' 'bpstep' object.
#'
#' @param step The name of the step
#' @param bp A 'blueprint' object to create the assembled step
#' @param payload A 'bpstep_payload' object that outlines the code
#'   to be assembled depending on the workflow executor
#' @param ... Extensions to the bpstep, like "allow_duplicates"
#'
#' @return A 'bpstep' object
#' @export
bpstep <- function(step, bp, payload, ...) {
  stopifnot(is_blueprint(bp))
  stopifnot(is.character(step))

  structure(
    list(
      step = step,
      blueprint = bp,
      payload = payload,
      ...
    ),
    class = "bpstep"
  )
}

is_bpstep <- function(x) {
  inherits(x, "bpstep")
}

#' @export
print.bpstep <- function(x, executor = NULL, ...) {
  cat_line("<blueprint assembly step>") # nocov start
  cat_line("name: {x$step}", indent = 1)
  cat_line("blueprint: '{x$blueprint$name}'", indent = 1)
  cat_line()
  print(x$payload, executor = executor)

  invisible(x) # nocov end
}

#' @export
print.assembled_bpstep <- function(x, ...) {
  cat_line("<assembled blueprint step>") # nocov start
  cat_line("name: {x$step}", indent = 1)
  cat_line("blueprint: '{x$blueprint$name}'", indent = 1)
  cat_line()
  cat_line("built payload:")
  print(x$built_payload)

  invisible(x) # nocov end
}

#' Create a step payload
#'
#' The bpstep payload is the object that contains the target name
#' and command, along with any other metadata to be passed to the
#' execution engine.
#'
#' @param target_name The target's name
#' @param target_command The target's command
#' @param ... Arguments to be passed to the executing engine (e.g.
#'   arguments sent to targets::tar_target())
#' @return A bpstep payload object
#' @export
#' @examples
#' if (FALSE) {
#'   bpstep(
#'     step = "some_step",
#'     bp = some_bp_object,
#'     payload = bpstep_payload(
#'       "payload_name",
#'       payload_command()
#'     )
#'   )
#' }
bpstep_payload <- function(target_name, target_command, ...) {
  bp_assert(is.character(target_name))
  bp_assert(is.language(target_command))

  structure(
    list(
      target_name = as.character(target_name),
      target_command = target_command,
      ...
    ),
    class = "bpstep_payload"
  )
}

#' @export
print.bpstep_payload <- function(x, executor = NULL, ...) {
  cat_line("<blueprint assembly step payload>") # nocov start
  cat_line("target: {x$target_name}")
  cat_line("command:")
  print(x$target_command)

  if (!is.null(executor)) {
    known_executors <- c("targets", "drake")

    if (!executor %in% known_executors) {
      cat_line("Unknown executor -- no preview of built payload")
    } else {
      asm <- switch(executor,
        targets = targets_assembler(),
        drake = drake_assembler()
      )

      print(assemble_payload(asm, x))
    }
  }

  invisible(x) # nocov end
}

bpstep_payload_extra_args <- function(payload) {
  bp_assert(inherits(payload, "bpstep_payload"))

  payload[!names(payload) %in% c("target_name", "target_command")]
}