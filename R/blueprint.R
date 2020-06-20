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

blueprint_target_name <- function(x, ...) {
  UseMethod("blueprint_target_name")
}

blueprint_target_name.default <- function(x) {
  bp_err("Not defined")
}

blueprint_target_name.character <- function(x) {
  paste0(x, "_initial")
}

blueprint_target_name.blueprint <- function(blueprint) {
  blueprint_target_name(blueprint$name)
}

blueprint_checks_name <- function(x, ...) {
  UseMethod("blueprint_checks_name")
}

blueprint_checks_name.default <- function(x, ...) {
  bp_err("Not defined")
}

blueprint_checks_name.character <- function(x) {
  paste0(x, "_checks")
}

blueprint_checks_name.blueprint <- function(blueprint) {
  blueprint_checks_name(blueprint$name)
}

blueprint_final_name <- function(x, ...) {
  UseMethod("blueprint_final_name")
}

blueprint_final_name.default <- function(x, ...) {
  bp_err("Not defined")
}

blueprint_final_name.character <- function(x) {
  x
}

blueprint_final_name.blueprint <- function(blueprint) {
  blueprint_final_name(blueprint$name)
}

blueprint_reference_name <- function(x, ...) {
  UseMethod("blueprint_reference_name")
}

blueprint_reference_name.default <- function(x, ...) {
  bp_err("Not defined")
}

blueprint_reference_name.character <- function(x) {
  paste0(x, "_blueprint")
}

blueprint_reference_name.blueprint <- function(blueprint) {
  blueprint_reference_name(blueprint$name)
}
