check_content <- function(df, blueprint, meta) {
  test_results <- data.table(test = character(), pass = logical(), messages = list())

  var_presence <- results_dt("variable_presence", check_variable_presence(df, meta))
  test_results <- rbindlist(list(test_results, var_presence))

  test_results
}

accept_content <- function(results, blueprint) {
  if (any(results$pass) == FALSE) {
    bp_err(c(
      "'{blueprint$name}' content checks failed. ", 
      "See `{paste0(blueprint_target_name(blueprint), '_content_checks')}` for more information."
    ))
  }

  TRUE
}

blueprint_check_results <- function(blueprint_name, ...) {
  bp_name_chr <- as.character(substitute(blueprint_name))

  command <- call2("readd", as.name(paste0(bp_name_chr, "_content_checks")), ..., .ns = "drake")

  eval(command)
}

check_variable_presence <- function(df, meta, stop_on_new_vars = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(inherits(meta, "blueprint_metadata"))

  known_vars <- meta$name

  new_vars <- setdiff(names(df), known_vars)
  missing_vars <- setdiff(known_vars, names(df))

  msg_payload <- blueprint_test_result()

  if (length(missing_vars) > 0) {
    msg_payload <- add_test_message(
      msg_payload,
      glue("Expected variables are missing: {glue_collapse(missing_vars, ', ')}")
    )
  }

  if (length(new_vars) > 0) {
    msg_payload <- add_test_message(
      msg_payload,
      glue("Unexpected new variables: {glue_collapse(new_vars, ', ')}"),
      "Please edit documentation if this is intended."
    )
  }

  if (length(missing_vars) > 0) {
    return(reject(msg_payload))
  }

  if (length(new_vars) > 0 && stop_on_new_vars == TRUE) {
    return(reject(msg_payload))
  }

  accept(msg_payload)
}

check_variable_types <- function(df, types_df) {
  stopifnot(is.data.frame(df))
  stopifnot(is.data.frame(types_df))

  msg_payload <- blueprint_test_result()
}
