has_uuid_attr <- function(obj) {
  has_attr(obj, ".uuid")
}

has_uuid_parents_attr <- function(obj) {
  has_attr(obj, ".uuid_parents")
}

clear_uuid_parents_attr <- function(obj) {
  set_attr(obj, ".uuid_parents", NULL)
}

uuid_attr <- function(obj) {
  get_attr(obj, ".uuid")
}

table_uuid_attrs <- function(df) {
  vcapply(df, uuid_attr)
}

restore_table_uuid_attrs <- function(df, table_uuids) {
  for (i in seq_along(df)) {
    df[[i]] <- set_attr(
      df[[i]],
      ".uuid",
      if (is.na(table_uuids[[i]])) NULL else table_uuids[[i]]
    )
  }

  df
}

uuid_parents_attr <- function(obj) {
  get_attr(obj, ".uuid_parents")
}

table_uuid_parents_attrs <- function(df) {
  vcapply(df, function(x) uuid_parents_attr(x) %||% NA_character_)
}

reset_uuid_attr <- function(obj) {
  set_attr(obj, ".uuid", uuid::UUIDgenerate())
}

has_multi_uuid <- function(obj) {
  grepl("\\|", uuid_attr(obj))
}

add_variable_uuids <- function(df) {
  for (nv in names(df)) {
    v <- df[[nv]]

    if (!has_uuid_attr(v)) {
      df[[nv]] <- reset_uuid_attr(v)
    } else if (has_uuid_attr(v) && has_multi_uuid(v)) {
      df[[nv]] <- set_attr(v, ".uuid_parents", uuid_attr(v))
      df[[nv]] <- reset_uuid_attr(df[[nv]])
    } else if (has_uuid_attr(v) && !has_multi_uuid(v) && has_uuid_parents_attr(v)) {
      df[[nv]] <- clear_uuid_parents_attr(v)
    }
  }

  df
}
