import_blueprint_file <- function(bp_file, env = parent.frame()) {
    exprs <- parse_exprs(file(bp_file))
    vals <- lapply(exprs, eval_tidy, env = env)

    if (length(vals) < 1) {
        abort(paste0("Blueprint script '", bp_file, "' has no content."))
    }

    script_val <- vals[[length(vals)]]

    if (!check_content_type(script_val)) {
        abort(paste0(
            "Blueprint script '",
            bp_file,
            "' does not evaluate to a blueprint or list of blueprints."
        ))
    }

    script_val
}

check_content_type <- function(x) {
    inherits(x, "blueprint") || (
        is.list(x) &&
        all(vlapply(x, function(.x) inherits(.x, "blueprint")))
    )
}
