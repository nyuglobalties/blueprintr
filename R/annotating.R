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

#' @describeIn annotations Gets a list of all annotations assigned to an object
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
#'   annotations assigned to an object
#' @export
annotation_names <- function(x) {
  if (is.null(attributes(x))) {
    return(character())
  }

  bp_attribs <- annotation_attribs(x)
  gsub(annotation_ns_pat, "", bp_attribs)
}

#' @describeIn annotations Gets an annotation for an object
#' @export
annotation <- function(x, field) {
  get_attr(x, annotation_key(field))
}

#' @describeIn annotations Gets an annotation that overrides existing
#'   annotations
super_annotation <- function(x, field) {
  get_attr(x, annotation_key(field, super = TRUE))
}

#' @describeIn annotations Checks to see if an annotation exists for an object
#' @export
has_annotation <- function(x, field) {
  !is_missing(annotation(x, field))
}

#' @describeIn annotations Checks to see if an overriding
#'   annotation exists for an object
has_super_annotation <- function(x, field) {
  !is_missing(super_annotation(x, field))
}

#' @describeIn annotations Adds an annotation to an object,
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

#' @describeIn annotations Alias to `add_annotation(overwrite = TRUE)`
set_annotation <- function(x, field, value) {
  add_annotation(x, field, value, overwrite = TRUE)
}

#' @describeIn annotations Adds an overriding annotation to an
#'   object. Note that overriding annotations will overwrite
#'   previous assignments!
add_super_annotation <- function(x, field, value) {
  set_attr(x, annotation_key(field, super = TRUE), value)
}

#' @describeIn annotations Removes overriding annotation
remove_super_annotation <- function(x, field) {
  set_attr(x, annotation_key(field, super = TRUE), NULL)
}

annotation_key <- function(field, super = FALSE) {
  if (isTRUE(super)) {
    field <- paste0("super.", field)
  }

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

#' "Super Annotations"
#'
#' As of blueprintr 0.2.1, there is now the option for metadata files to
#' **always** overwrite annotations at runtime. Previously, this would be
#' a conflict with [blueprintr::mutate_annotation] and [blueprintr::mutate_annotation_across]
#' since the annotation phase happens during the blueprint cleanup phase, whereas these
#' annotation manipulation tools occur at the blueprint initial phase. To resolve
#' this, 0.2.1 introduces "super annotations", which are just annotations prefixed
#' with "super.". However, the super annotations will _overwrite_ the normal annotations
#' during cleanup. This gives the annotation manipulation tools a means of not losing their
#' work if `annotate_overwrite` is effectively enabled. To enable this functionality,
#' set `options(blueprintr.use_improved_annotations = TRUE)`. This also has the side effect
#' of **always** treating `annotate = TRUE` and `annotate_overwrite = TRUE`.
#'
#' @name super_annotations
NULL

#' @describeIn super_annotations Returns the option string for improved annotations
improved_annotation_option <- function() {
  "blueprintr.use_improved_annotations"
}

#' @describeIn super_annotations Checks if improved annotations are enabled
using_improved_annotations <- function() {
  getOption(improved_annotation_option(), default = FALSE)
}
