#' Create a blueprint
#'
#' @param name The name of the blueprint
#' @param command The code to build the target dataset
#' @param description An optional description of the dataset to be used for
#'                    codebook generation
#' @param metadata The associated variable metadata for this dataset
#' @param export_metadata Indicator to render new metadata file for new dataset
#' @param metadata_file_type
#' @param metadata_file_path
#' @param ...
#' @param class
#'
#' @return
#' @export
blueprint <- function(name,
                      command,
                      description = NULL,
                      metadata = NULL,
                      export_metadata = TRUE,
                      metadata_file_type = c("csv"),
                      metadata_file_path = here::here("blueprints"),
                      ...,
                      class = character()) {
  stopifnot(is.character(name))
  stopifnot(is.null(description) || is.character(description))

  captured_command <- capture_command(substitute(command))
  metadata_file_type <- match.arg(metadata_file_type)

  structure(
    list(
      name = name,
      command = captured_command,
      description = description,
      export_metadata = export_metadata,
      metadata_file_type = metadata_file_type,
      metadata_file_path = metadata_file_path,
      ...
    ),
    class = c(class, "blueprint")
  )
}

capture_command <- function(quoted_statement) {
  if (identical(quote(.), node_car(quoted_statement))) {
    return(eval(node_cdr(quoted_statement)[[1]]))
  }

  quoted_statement
}

#' @export
blueprint_target_name <- function(x, ...) {
  UseMethod("blueprint_target_name")
}

#' @export
blueprint_target_name.default <- function(x) {
  bp_err("Not defined")
}

#' @export
blueprint_target_name.character <- function(x) {
  paste0(x, "_initial")
}

#' @export
blueprint_target_name.blueprint <- function(blueprint) {
  blueprint_target_name(blueprint$name)
}

#' @export
blueprint_checks_name <- function(x, ...) {
  UseMethod("blueprint_checks_name")
}

#' @export
blueprint_checks_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @export
blueprint_checks_name.character <- function(x) {
  paste0(x, "_checks")
}

#' @export
blueprint_checks_name.blueprint <- function(blueprint) {
  blueprint_checks_name(blueprint$name)
}

#' @export
blueprint_final_name <- function(x, ...) {
  UseMethod("blueprint_final_name")
}

#' @export
blueprint_final_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @export
blueprint_final_name.character <- function(x) {
  x
}

#' @export
blueprint_final_name.blueprint <- function(blueprint) {
  blueprint_final_name(blueprint$name)
}

#' @export
blueprint_reference_name <- function(x, ...) {
  UseMethod("blueprint_reference_name")
}

#' @export
blueprint_reference_name.default <- function(x, ...) {
  bp_err("Not defined")
}

#' @export
blueprint_reference_name.character <- function(x) {
  paste0(x, "_blueprint")
}

#' @export
blueprint_reference_name.blueprint <- function(blueprint) {
  blueprint_reference_name(blueprint$name)
}
