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

blueprint_target_name <- function(blueprint) {
    stopifnot(inherits(blueprint, "blueprint"))

    paste0(blueprint$name, "_initial")
}

blueprint_checks_name <- function(blueprint) {
    stopifnot(inherits(blueprint, "blueprint"))

    paste0(blueprint$name, "_checks")
}

blueprint_final_name <- function(blueprint) {
    stopifnot(inherits(blueprint, "blueprint"))

    blueprint$name
}

blueprint_reference_name <- function(blueprint) {
    stopifnot(inherits(blueprint, "blueprint"))

    paste0(blueprint$name, "_blueprint")
}
