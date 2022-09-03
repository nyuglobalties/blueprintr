#' Variable lineage
#'
#' This is an experimental feature that traces variable lineage through
#' an injection of a ".uuid" attribute for each variable. Previous attempts
#' at variable lineage were conducted using variable names and heuristics
#' of known functions. This approach yields a more consistent lineage.
#'
#' To enable the variable feature, set `options(blueprintr.use_variable_uuids = TRUE)`.
#'
#' @param directory A folder containing blueprint scripts
#' @param recurse Should this function recursively load blueprints?
#' @param script Where the targets/drake project script file is located. Defaults
#'   to using targets.
#' @param ... Arguments passed to [blueprintr::load_variable_lineage]
#' @param g An igraph object. This defaults to a graph loaded with
#'   [blueprintr::load_variable_lineage]. However, use this if you
#'   want to inspect subgraphs of the variable lineage.
#' @param cluster_by_dataset If `TRUE`, variable nodes will be clustered into their
#'   respective dataset
#' @name variable_lineage
NULL

#' @describeIn variable_lineage Reads blueprintrs from folder to get variable lineage.
#'   Returns an igraph of the variable lineage.
#' @export
load_variable_lineage <- function(directory = here::here("blueprints"),
                                  recurse = FALSE,
                                  script = here::here("_targets.R")) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    bp_err(c(
      "Viewing the variable lineage and other provenance features requires ",
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

      get_variable_linage_igraph(bp_list)
    },
    args = list(script = script, directory = directory, recurse = recurse),
    package = "blueprintr"
  )
}


#' Get an igraph of the variable lineage
#'
#' @param blueprints a list() of blueprint objects
#' @return An igraph object of the variable lineage structure
get_variable_linage_igraph <- function(blueprints) {
  dep_tables <- lapply(blueprints, blueprint_variable_dep_tables)
  dep_nodes <- lapply(dep_tables, function(x) x[["node"]])
  dep_edges <- lapply(dep_tables, function(x) x[["edges"]])

  acc_node <- do.call(rbind, dep_nodes)
  acc_edges <- do.call(rbind, dep_edges)

  if (any(duplicated(acc_node$id))) {
    bad_hashes <- acc_node[
      duplicated(acc_node$id) |
        duplicated(acc_node$hash, fromLast = TRUE),
    ]

    bp_err(
      c(
        "There are variables that have duplicate UUIDs! ",
        "This usually happens when using non-attribute-safe functions.\n",
        "See `.Last.error$parent$error$variables` for more details."
      ),
      variables = bad_hashes
    )
  }

  igraph::graph_from_data_frame(acc_edges, directed = TRUE, vertices = acc_node)
}

#' @describeIn variable_lineage Visualizes variable lineage with visNetwork.
#'   Returns an interactive graph.
#' @export
vis_variable_lineage <- function(..., g = NULL, cluster_by_dataset = TRUE) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    bp_err("Please install 'visNetwork' to create an interactive graph of table lineage")
  }

  g <- g %||% load_variable_lineage(...)

  # Make properties compatible with visNetwork
  igraph::V(g)$group <- igraph::V(g)$database

  vis_g <- visNetwork::toVisNetworkData(g)
  vis_g$nodes$label <- vis_g$nodes$varname

  vis_g <- visNetwork::visNetwork(nodes = vis_g$nodes, edges = vis_g$edges)
  vis_g <- visNetwork::visEdges(
    vis_g,
    arrows = "to",
    smooth = list(
      type = "cubicBezier",
      forceDirection = "horizontal"
    )
  )

  if (isTRUE(cluster_by_dataset)) {
    vis_g <- visNetwork::visClusteringByGroup(
      vis_g,
      groups = unique(vis_g$x$nodes$group),
      label = "Table: "
    )
  }

  visNetwork::visLegend(vis_g)
}

blueprint_variable_dep_table_node <- function() {
  data.frame(
    id = character(), uuid = character(),
    varname = character(), database = character(),
    parents = character()
  )
}

blueprint_variable_dep_table_edges <- function() {
  data.frame(
    from = character(),
    to = character(),
    uuid = character()
  )
}

blueprint_variable_dep_tables <- function(bp, dat = NULL, deps = NULL, ...) {
  if (is.null(dat) && !requireNamespace("targets", quietly = TRUE)) {
    bp_err("Variable lineage is currently only supported with targets projects")
  }

  dat <- dat %||% targets::tar_read_raw(bp$name, ...)
  bp_deps <- blueprint_target_deps(bp)

  if (length(bp_deps) > 0) {
    if (is.null(deps)) {
      deps <- vl_populate_deps(bp_deps, ...)
    } else {
      bp_assert(setequal(names(deps), bp_deps))
    }
  }

  dat_table <- vl_dat_table(dat, bp$name)
  edges <- vl_edge_table(dat_table, deps)

  list(
    node = dat_table,
    edges = edges
  )
}

vl_populate_deps <- function(bp_deps, ...) {
  deps <- vector("list", length(bp_deps))
  names(deps) <- bp_deps

  for (ndep in bp_deps) {
    deps[[ndep]] <- targets::tar_read_raw(ndep, ...)
  }

  deps
}

vl_dat_table <- function(dat, datname) {
  uuids <- table_uuid_attrs(dat)
  uuid_parents <- table_uuid_parents_attrs(dat)

  ids <- vcapply(uuids, function(x) digest::digest(c(datname, x), algo = "xxhash32"))

  data.frame(
    id = ids,
    uuid = uuids,
    varname = names(dat),
    database = datname,
    parents = uuid_parents,
    row.names = NULL
  )
}

vl_edge_table <- function(dat_table, deps) {
  dep_table <- vl_dep_table_from_deps(deps)
  edges <- vl_parent_tables(dat_table, dep_table)

  if (any(!is.na(dat_table$parents))) {
    edges_variables <- vl_parent_vars(dat_table, dep_table)
    edges <- rbind(edges, edges_variables)
  }

  edges
}

vl_dep_table_from_deps <- function(deps) {
  dep_table <- blueprint_variable_dep_table_node()

  if (length(deps) > 0) {
    for (ndep in names(deps)) {
      dep_table <- rbind(dep_table, vl_dat_table(deps[[ndep]], ndep))
    }
  }

  dep_table
}

vl_parent_tables <- function(dat_table, dep_table) {
  edges <- blueprint_variable_dep_table_edges()

  if (nrow(dep_table) < 1) {
    return(edges)
  }

  sub_dat <- dat_table[, c("uuid", "id")]
  names(sub_dat) <- c("uuid", "to")

  sub_dep_dat <- dep_table[, c("uuid", "id")]
  names(sub_dep_dat) <- c("uuid", "from")

  merged <- merge(sub_dat, sub_dep_dat, by = "uuid")

  edges <- merged[, c("from", "to", "uuid")]
  edges
}

vl_parent_vars <- function(dat_table, dep_table) {
  parent_dat_table <- dat_table[!is.na(dat_table$parents), c("id", "uuid", "parents")]

  parent_dat_table_long <- dplyr::group_by(parent_dat_table, .data$uuid)
  parent_dat_table_long <- dplyr::summarise(
    parent_dat_table_long,
    id = unique(.data$id),
    parents = unlist(strsplit(.data$parents, "\\|")),
    .groups = "drop"
  )

  # Choose current table variables over dependency tables' variables
  # to preserve correct order of provenance
  edges <- parent_dat_table_long[, c("parents", "id")]
  names(edges) <- c("uuid", "to")

  dat_edges <- dat_table[, c("uuid", "id")]
  names(dat_edges) <- c("uuid", "from_dat")

  dep_edges <- dep_table[, c("uuid", "id")]
  names(dep_edges) <- c("uuid", "from_dep")

  edges <- merge(edges, dat_edges, by = "uuid", all.x = TRUE)
  edges <- merge(edges, dep_edges, by = "uuid", all.x = TRUE)

  edges$from <- dplyr::coalesce(edges$from_dat, edges$from_dep)
  edges$from_dat <- NULL
  edges$from_dep <- NULL

  edges[, c("from", "to", "uuid")]
}

variable_uuid_option <- function() {
  "blueprintr.use_variable_uuids"
}

using_variable_uuids <- function() {
  getOption(variable_uuid_option(), default = FALSE)
}
