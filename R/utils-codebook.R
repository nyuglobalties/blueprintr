paragraph <- function(x) {
  cat_line(x)
  cat_line()
}

h_tag <- function(x, level) {
  header_prefix <- paste0(rep("#", level))

  paragraph(c(header_prefix, " ", x))
}

h1 <- function(x) {
  h_tag(x, 1)
}

h2 <- function(x) {
  h_tag(x, 2)
}

h3 <- function(x) {
  h_tag(x, 3)
}

h4 <- function(x) {
  h_tag(x, 4)
}

echo_list <- function(x, ordered = FALSE) {
  bullet <- if (ordered) "1. " else "* "

  x <- paste0(bullet, x, "\n")
  paragraph(x)
}

unordered_list <- function(x) {
  echo_list(x, ordered = FALSE)
}

ordered_list <- function(x) {
  echo_list(x, ordered = TRUE)
}

blockquote <- function(x) {
  paragraph(paste0("> ", x))
}

codeblock <- function(x) {
  cat_line("```")
  print(x)
  paragraph("```")
}

span_tag <- function(x, class = NULL) {
  if (!is.null(class)) {
    cat_line(glue::glue('<span class="{class}">{x}</span>'))
  } else {
    cat_line(glue::glue("<span>{x}</span>"))
  }
}

badge <- function(x, type = "info") {
  span_tag(x, class = paste0("label label-", type))
}

is_empty_text <- function(x) {
  is.na(x) | x == ""
}

remove_dropped <- function(meta_dt) {
  if ("dropped" %in% names(meta_dt)) {
    meta_dt <- tidytable::filter(meta_dt, is.na(.data$dropped) | .data$dropped == FALSE)
  }

  as.data.frame(meta_dt)
}

create_coding_list <- function(meta_dt) {
  if ("coding" %in% names(meta_dt)) {
    meta_dt <- tidytable::mutate(
      meta_dt,
      .evaluated_coding = rcoder::as_coding_list(.data$coding)
    )
  }

  as.data.frame(meta_dt)
}

arrange_by_sections <- function(meta_dt) {
  meta <- tidytable::mutate(
    meta_dt,
    section = ifelse(is_empty_text(.data$section), "Other", .data$section)
  )

  meta_categorized <- tidytable::filter(meta, .data$section != "Other")
  meta_uncategorized <- tidytable::filter(meta, .data$section == "Other")

  meta_categorized <- tidytable::mutate(
    meta_categorized,
    .section_order = viapply(
      .data$section,
      function(.x) which(unique(.data$section) == .x)
    )
  )
  meta_categorized <- tidytable::arrange(meta_categorized, .data$.section_order)
  meta_categorized <- tidytable::mutate(meta_categorized, .section_order = NULL)

  as.data.frame(tidytable::bind_rows(meta_categorized, meta_uncategorized))
}

variable_tags <- function(variable, meta_dt) {
  var_tags <- tidytable::filter(meta_dt, .data$name == variable)[["tags"]]

  if (is.na(var_tags)) {
    return(character())
  }

  strsplit(var_tags, "\\,")[[1]]
}

coding_table <- function(coding) {
  coding_df <- as.data.frame(coding)
  coding_df <- tidytable::select(coding_df, .data$label, .data$value)
  coding_df <- tidytable::filter(coding_df, !duplicated(.data$label))

  coding_df <- kableExtra::kable(as.data.frame(coding_df))
  coding_df <- kableExtra::kable_styling(
    coding_df,
    bootstrap_options = "striped",
    full_width = FALSE,
    position = "float_right"
  )

  print(coding_df)
}

write_variable <- function(variable, meta, data, in_group = FALSE) {
  var_desc <- tidytable::filter(meta, .data$name == variable)[["description"]]
  var_title <- NULL

  if ("title" %in% names(meta)) {
    var_title <- tidytable::filter(meta, .data$name == variable)[["title"]]

    if (is_empty_text(var_title)) {
      var_title <- NULL
    }
  }

  if ("coding" %in% names(meta)) {
    var_coding <- tidytable::filter(meta, .data$name == variable)[[".evaluated_coding"]][[1]]

    if (rcoder::is_empty_coding(var_coding)) {
      var_coding <- NULL
    }
  } else {
    var_coding <- NULL
  }

  if (isTRUE(in_group)) {
    h_level <- h4
  } else {
    h_level <- h3
  }

  if (!is.null(var_title)) {
    h_level(paste0(variable, " --- ", var_title))
  } else {
    h_level(variable)
  }

  if (!is.null(var_coding)) {
    coding_table(var_coding)
  }

  if (!is_empty_text(var_desc)) {
    blockquote(var_desc)
  }

  if (!is.null(data)) {
    if (!is.character(data[[variable]])) {
      codeblock(summary(data[[variable]]))
    }
  }

  if ("tags" %in% names(meta)) {
    var_tags <- variable_tags(variable, meta)

    if (length(var_tags) > 0) {
      for (tag in var_tags) {
        badge(tag)
      }

      cat_line()
    }
  }

  paragraph("---")
  invisible(NULL)
}

write_group <- function(.group, meta, data) {
  meta <- tidytable::filter(meta, .data$group == .group)

  if ("group_description" %in% names(meta)) {
    group_description <- unique(meta$group_description)[1]

    if (is_empty_text(group_description)) {
      group_description <- NULL
    }
  } else {
    group_description <- NULL
  }

  h3(.group)

  if (!is.null(group_description)) {
    paragraph(paste0("*", group_description, "*"))
  }
}

write_grouped_variables <- function(meta, data) {
  # Iterate through rows instead of variable names
  current_group <- ""
  echoed <- FALSE

  for (i in 1:nrow(meta)) {
    variable <- meta[i, ][["name"]]
    group <- meta[i, ][["group"]]

    if (!is_empty_text(group)) {
      if (!identical(group, current_group)) {
        echoed <- FALSE
      }
    }

    current_group <- group
    in_grp <- !is_empty_text(current_group)

    if (in_grp && !isTRUE(echoed)) {
      write_group(current_group, meta, data)
      echoed <- TRUE
    }

    write_variable(variable, meta, data, in_group = in_grp)
  }
}

write_variables <- function(meta, data) {
  if ("group" %in% names(meta)) {
    write_grouped_variables(meta, data)
  } else {
    for (variable in meta$name) {
      write_variable(variable, meta, data)
    }
  }
}

write_section <- function(.section, meta, data) {
  meta <- meta[meta$section == .section, ]

  if ("section_description" %in% names(meta)) {
    section_description <- unique(meta$section_description)[1]

    if (is_empty_text(section_description)) {
      section_description <- NULL
    }
  } else {
    section_description <- NULL
  }

  h2(.section)

  if (!is.null(section_description)) {
    paragraph(section_description)
  }

  write_variables(meta, data)
}

write_variables_only <- function(meta, data) {
  meta <- remove_dropped(meta)
  meta <- create_coding_list(meta)

  write_variables(meta, data)
}

write_sections <- function(meta, data) {
  meta <- remove_dropped(meta)
  meta <- arrange_by_sections(meta)
  meta <- create_coding_list(meta)

  for (section in unique(meta$section)) {
    write_section(section, meta, data)
  }
}
