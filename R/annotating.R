#' Access the blueprintr metadata at runtime
#' 
#' @param x An object, most likely a variable in a `data.frame`
#' @param field The name of a metadata field
#' @param value A value to assign to an annotation field
#' @param overwrite If `TRUE`, allows overwriting of existing
#'   annotation values
#' @name annotations
#' @export
NULL

annotation_ns <- "bpr"
annotation_ns_pat <- paste0("^", annotation_ns, "\\.")

#' @describeIn annotations Gets a list of all decorations assigned to an object
#' @export
annotations <- function(x) {
  if (is.null(attributes(x))) {
    return(NULL)
  }

  matched <- attributes(x)[annotation_attribs(x)]
  names(matched) <- gsub(annotation_ns_pat, "", names(matched))

  if (length(matched) < 1) {
    matched <- NULL
  }
  
  matched
}

annotation_attribs <- function(x) {
  attrib_names <- names(attributes(x))
  grep(annotation_ns_pat, attrib_names, value = TRUE)
}

#' @describeIn annotations Get the names of all of the 
#'   decorations assigned to an object
#' @export
annotation_names <- function(x) {
  if (is.null(attributes(x))) {
    return(character())
  }

  bp_attribs <- annotation_attribs(x)
  gsub(annotation_ns_pat, "", bp_attribs)
}

#' @describeIn annotations Gets a decoration for an object
#' @export
annotation <- function(x, field) {
  get_attr(x, annotation_key(field))
}

#' @describeIn annotations Checks to see if a decoration exists for an object
#' @export
has_annotation <- function(x, field) {
  !is_missing(annotation(x, field))
}

#' @describeIn annotations Adds a decoration to an object, 
#'   with the option of overwriting an existing value
#' @export
add_annotation <- function(x, field, value, overwrite = FALSE) {
  old_val <- annotation(x, field)

  if (!is_missing(old_val) && !isTRUE(overwrite)) {
    return(x)
  }

  if (is_missing(value)) {
    return(x)
  }

  set_attr(x, annotation_key(field), value)
}

annotation_key <- function(field) {
  paste0(annotation_ns, ".", field)
}

annotation_table_df <- function(df) {
  stopifnot(is.data.frame(df))

  dec_dats <- lapply(names(df), function(nx) {
    annotation_table(df[[nx]], nx)
  })

  dplyr::bind_rows(!!!dec_dats)
}

annotation_table <- function(x, nx) {
  stopifnot(is.character(nx))

  dec_dat <- dplyr::tibble(name = nx)

  if (length(annotation_names(x)) == 0) {
    return(dec_dat)
  }

  for (dec in annotation_names(x)) {
    dec_dat[[dec]] <- annotation(x, dec)
  }

  dec_dat
}