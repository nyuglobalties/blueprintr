metadata <- function(df) {
    stopifnot(is.data.frame(df))

    # Metadata MUST have name, description, and type columns at least
    req_columns <- c("name", "description", "type")
    if (any(!req_columns %in% names(df))) {
        missing_columns <- req_columns[!req_columns %in% names(df)]

        abort(paste(
            "Required blueprint metadata columns", 
            collapse_message_list(missing_columns),
            "not found."
        ))
    }

    class(df) <- c(class(df), "blueprint_metadata")
    df
}

clean_tests_column <- function(x) {
    if (is.character(x)) {
        
    }
}
