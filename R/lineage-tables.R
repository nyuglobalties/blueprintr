#' Read blueprints from folder and get lineage
#'
#' @param directory A folder containing blueprint scripts
#' @param recurse Should this function recursively load blueprints?
#' @param script Where the targets/drake project script file is located. Defaults
#'   to using targets.
#' @return An igraph of the table lineage for the desired blueprints
#' @export
load_table_lineage <- function(directory = here::here("blueprints"),
                               recurse = FALSE,
                               script = here::here("_targets.R")) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    bp_err(c(
      "Viewing the table lineage and other provenance features requires ",
      "installing the 'igraph' package. It should already be installed if ",
      "you have either 'drake' or 'targets' installed, but please re-install."
    ))
  }

  if (!requireNamespace("callr", quietly = TRUE)) {
    bp_err("callr needed to run project file in separate process")
  }

  callr::r(
    function(script, directory, recurse) {
      source(script)

      dirs <- load_dirs_recurse(directory, recurse)
      bp_list <- fetch_blueprints_from_dir(dirs)

      get_table_linage_igraph(bp_list)
    },
    args = list(script = script, directory = directory, recurse = recurse),
    package = "blueprintr"
  )
}

#' Visualize table lineage with visNetwork
#'
#' @param ... Arguments passed to [blueprintr::load_table_lineage]
#' @return Interactive graph run by visNetwork
#' @export
vis_table_lineage <- function(...) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    bp_err("Please install 'visNetwork' to create an interactive graph of table lineage")
  }

  g <- load_table_lineage(...)

  vis_g <- visNetwork::toVisNetworkData(g)
  vis_g <- visNetwork::visNetwork(nodes = vis_g$nodes, edges = vis_g$edges)
  vis_g <- visNetwork::visEdges(
    vis_g,
    arrows = "to",
    smooth = list(
      type = "cubicBezier",
      forceDirection = "horizontal"
    )
  )
  vis_g <- visNetwork::visOptions(
    vis_g,
    collapse = TRUE,
    highlightNearest = list(
      enabled = TRUE,
      algorithm = "hierarchical"
    )
  )
  visNetwork::visHierarchicalLayout(vis_g, direction = "LR", sortMethod = "directed")
}

#' Get an igraph of the table lineage
#'
#' @param blueprints a list() of blueprint objects
#' @return An igraph object of the table lineage structure
get_table_linage_igraph <- function(blueprints) {
  acc_node <- blueprint_dependency_table_node()
  acc_edges <- blueprint_dependency_table_edges()

  for (bp in blueprints) {
    dep_tables <- blueprint_dependency_tables(bp)
    acc_node <- rbind(acc_node, dep_tables$node)
    acc_edges <- rbind(acc_edges, dep_tables$edges)
  }

  # Create visNetwork tooltip
  acc_node <- tl_create_visnetwork_tooltip(acc_node)

  igraph::graph_from_data_frame(acc_edges, directed = TRUE, vertices = acc_node)
}

tl_create_visnetwork_tooltip <- function(acc_node) {
  header <- ifelse(is.na(acc_node$description), acc_node$name, acc_node$description)

  acc_node$title <- paste0(
    "<strong>", header, "</strong><br /><br />",
    "Metadata:</br>", acc_node$metadata_path
  )

  acc_node
}

blueprint_dependency_tables <- function(bp) {
  bp_assert(is_blueprint(bp))

  node <- blueprint_dependency_table_node(bp)
  edges <- blueprint_dependency_table_edges(bp)

  list(
    node = node,
    edges = edges
  )
}

blueprint_dependency_table_node <- function(bp = NULL) {
  node <- data.frame(
    name = character(),
    type = character(),
    description = character(),
    metadata_path = character()
  )

  if (!is.null(bp)) {
    bp_node <- data.frame(
      name = bp$name,
      type = class(bp)[1],
      description = bp$description %||% NA_character_,
      metadata_path = bp$metadata_file_path
    )

    node <- rbind(node, bp_node)
  }

  node
}

blueprint_dependency_table_edges <- function(bp = NULL) {
  edges <- data.frame(
    from = character(),
    to = character()
  )

  if (!is.null(bp)) {
    target_deps <- blueprint_target_deps(bp)

    if (length(target_deps) > 0) {
      bp_edges <- data.frame(
        from = target_deps,
        to = rep(bp$name, length(target_deps))
      )

      edges <- rbind(edges, bp_edges)
    }
  }

  edges
}

blueprint_target_deps <- function(bp) {
  bp_assert(inherits(bp, "blueprint"))

  command_ast <- extract_ast(bp$command)

  target_calls <- find_ast_if(command_ast, target_call_check)
  target_names <- flatten_deps_search_stack(target_calls)

  if (is.null(target_names)) {
    return(character())
  }

  unname(target_names[target_names != ".TARGET"])
}

blueprint_source_deps <- function(bp) {
  bp_assert(inherits(bp, "blueprint"))

  command_ast <- extract_ast(bp$command)

  source_calls <- find_ast_if(command_ast, source_call_check)
  source_names <- flatten_deps_search_stack(source_calls)

  if (is.null(source_names)) {
    return(character())
  }

  unname(source_names[source_names != ".SOURCE"])
}
