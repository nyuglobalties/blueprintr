blueprint <- function(name, 
                      command,
                      description = NULL,
                      metadata = NULL,
                      export_metadata = TRUE,
                      metadata_file_type = c("csv"), 
                      ..., 
                      class = character()) {
    stopifnot(is.character(name))
    stopifnot(is.null(description) || is.character(description))

    captured_command <- substitute(command)
    metadata_file_type <- match.arg(metadata_file_type)

    structure(
        list(
            name = name,
            command = captured_command,
            description = description,
            export_metadata = export_metadata,
            metadata_file_type = metadata_file_type,
            ...
        ),
        class = c(class, "blueprint")
    )
}
