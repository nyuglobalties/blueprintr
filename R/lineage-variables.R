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
#' @param variables Character vector of patterns for variable names to
#'   match. Note that each pattern is assumed to be disjoint (e.g. "if variable pattern
#'   A _or_ variable pattern B"), but if `tables` is not `NULL`, the search will be joint
#'   (e.g. "if (variable pattern A _or_ variable pattern B) _and_ (table pattern A _or_
#'   table pattern B)").
#' @param tables Character vector of patterns for table names to match. Note that
#'   each pattern is assumed to be disjoint (e.g. "if table pattern A _or_ table pattern B"),
#'   but if `variables` is not `NULL`, the search will be joint (e.g. "if (table pattern A
#'   _or_ table pattern B) _and_ (variable pattern A _or_ variable pattern B)").
#' @param mode Which sort of relationships to include. Defaults to "all" (includes both
#'   relations _to_ the target node in the graph and _from_ the target node in the graph).
#'   See [igraph::all_simple_paths()][igraph::all_simple_paths] for more details.
#' @param cutoff The number of node steps to consider in the graph traversal for filtering.
#'   Defaults to -1 (no limit on steps). See [igraph::all_simple_paths()][igraph::all_simple_paths]
#'   for more details.
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

  bp_list <- callr::r(
    function(script, directory, recurse) {
      source(script)

      dirs <- load_dirs_recurse(directory, recurse)
      fetch_blueprints_from_dir(dirs)
    },
    args = list(script = script, directory = directory, recurse = recurse),
    package = "blueprintr"
  )

  get_variable_lineage_igraph(bp_list)
}


#' Get an igraph of the variable lineage
#'
#' @param blueprints a list() of blueprint objects
#' @param dats A list of data.frames corresponding to each blueprint output.
#'   Only really to be used in testing.
#' @param deps A list of named lists of data.frames corresponding to the dependencies
#'   for each blueprint output. Only really to be used in testing.
#' @return An igraph object of the variable lineage structure
#' @noRd
get_variable_lineage_igraph <- function(blueprints, dats = NULL, deps = NULL) {
  dep_tables <- lapply(
    seq_along(blueprints),
    function(i) {
      blueprint_variable_dep_tables(
        blueprints[[i]],
        dat = dats[[i]],
        deps = deps[[i]]
      )
    }
  )

  dep_nodes <- lapply(dep_tables, function(x) x[["node"]])
  dep_deps <- lapply(dep_tables, function(x) x[["deps"]])
  dep_edges <- lapply(dep_tables, function(x) x[["edges"]])

  acc_node <- do.call(rbind, dep_nodes)
  acc_deps <- unique(do.call(rbind, dep_deps))
  acc_node <- unique(rbind(acc_node, acc_deps))
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

#' @describeIn variable_lineage Filter for specific variables to include
#'   in the lineage graph
#' @export
filter_variable_lineage <- function(g, variables = NULL, tables = NULL, mode = "all", cutoff = -1) {
  var_names <- character()
  var_dats <- character()

  for (var_pat in variables) {
    var_names <- c(
      var_names,
      match_varname_var(g, var_pat)
    )
  }

  for (dat_pat in tables) {
    var_dats <- c(
      var_dats,
      match_varname_dat(g, dat_pat)
    )
  }

  if (!is.null(variables) && !is.null(tables)) {
    var_names <- intersect(var_names, var_dats)
  } else {
    # If either NULL, keep mutually exclusive
    var_names <- if (length(var_names) > 0) unique(var_names) else unique(var_dats)
  }

  list_vars <- vector("list", length(var_names))
  names(list_vars) <- var_names

  for (v in var_names) {
    list_vars[[v]] <- igraph::all_simple_paths(g, from = v, mode = mode, cutoff = cutoff)
  }

  list_vars <- unlist(list_vars, recursive = FALSE)
  igraph::V(g)$keep <- FALSE

  for (vert in list_vars) {
    igraph::V(g)[vert]$keep <- TRUE
  }

  igraph::delete_vertex_attr(
    igraph::induced_subgraph(
      g,
      which(igraph::V(g)$keep == TRUE)
    ),
    "keep"
  )
}

match_varname_var <- function(g, pattern) {
  sub_vs <- igraph::V(g)[grepl(pattern, igraph::V(g)$varname)]
  sub_vs$name
}

match_varname_dat <- function(g, pattern) {
  sub_vs <- igraph::V(g)[grepl(pattern, igraph::V(g)$database)]
  sub_vs$name
}

#' @describeIn variable_lineage Visualizes variable lineage with visNetwork.
#'   Returns an interactive graph.
#' @export
vis_variable_lineage <- function(..., g = NULL, cluster_by_dataset = TRUE) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    bp_err("Please install 'visNetwork' to create an interactive graph of table lineage")
  }

  g <- g %||% load_variable_lineage(...)

  # Add table tooltip in case there is only one var in a selected table, which
  # drops the "Table: " label
  igraph::V(g)$title <- paste0(
    "<strong>", igraph::V(g)$varname, "</strong><br /><br />",
    "Table: ", igraph::V(g)$database
  )

  # Make properties compatible with visNetwork
  igraph::V(g)$group <- igraph::V(g)$database
  igraph::V(g)$shape <- ifelse(
    igraph::V(g)$database_type == "source",
    "square",
    "circle"
  )

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
      label = "Table: ",
      scale_size = FALSE
    )
  }

  visNetwork::visLegend(
    vis_g,
    useGroups = FALSE,
    addNodes = list(
      list(label = "Blueprint", shape = "circle", color = "lightblue", size = 25),
      list(label = "Source", shape = "square", color = "lightblue", size = 25)
    )
  )
}

blueprint_variable_dep_table_node <- function() {
  data.frame(
    id = character(), uuid = character(),
    varname = character(), database = character(),
    database_type = character(), parents = character()
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
  bp_source_deps <- blueprint_source_deps(bp)
  bp_all_deps <- c(bp_deps, bp_source_deps)

  bp_dep_types <- c(
    rep("blueprint", length(bp_deps)),
    rep("source", length(bp_source_deps))
  )
  names(bp_dep_types) <- bp_all_deps

  if (length(bp_all_deps) > 0) {
    if (is.null(deps)) {
      deps <- vl_populate_deps(bp_all_deps, ...)
    } else {
      bp_assert(setequal(names(deps), bp_all_deps))
    }
  }

  dat_table <- vl_dat_table(dat, bp$name)
  edge_tables <- vl_edge_table(dat_table, deps, types = bp_dep_types)

  list(
    node = dat_table,
    deps = edge_tables$dep_table,
    edges = edge_tables$edges
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

vl_dat_table <- function(dat, datname, type = NULL) {
  uuids <- table_uuid_attrs(dat)
  uuid_parents <- table_uuid_parents_attrs(dat)

  ids <- vcapply(uuids, function(x) digest::digest(c(datname, x), algo = "xxhash32"))

  data.frame(
    id = ids,
    uuid = uuids,
    varname = names(dat),
    database = datname,
    database_type = type %||% "blueprint",
    parents = uuid_parents,
    row.names = NULL
  )
}

vl_edge_table <- function(dat_table, deps, types = NULL) {
  dep_table <- vl_dep_table_from_deps(deps, types = types)
  edges <- vl_parent_tables(dat_table, dep_table)

  if (any(!is.na(dat_table$parents))) {
    edges_variables <- vl_parent_vars(dat_table, dep_table)
    edges <- rbind(edges, edges_variables)
  }

  list(
    edges = edges,
    dep_table = dep_table
  )
}

vl_dep_table_from_deps <- function(deps, types = NULL) {
  dep_table <- blueprint_variable_dep_table_node()

  if (length(deps) > 0) {
    for (ndep in names(deps)) {
      dep_table <- rbind(
        dep_table,
        vl_dat_table(deps[[ndep]], ndep, type = types[[ndep]])
      )
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
