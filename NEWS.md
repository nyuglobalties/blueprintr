# blueprintr 0.2.6
* Switch default workflow executor to targets
* Update vignettes to demonstrate current workflow procedure (@hgao1, #59)
* Remove dplyr dependency

# blueprintr 0.2.5
* Add capability to embed custom messages to check results, using `check.errors` attribute in returned logical value
* Refactor side-effect messages from built-in checks to `check.errors`

# blueprintr 0.2.4
* Fix for issue where labelling large datasets would take a very long time

# blueprintr 0.2.3
* Add ability to filter variable lineage with `blueprintr::filter_variable_lineage()`
* Add tooltop for variable node in variable lineage
* Fix issue where grouped nodes in visNetwork would scale, harming legibility
* Convert empty folder warning to option. Enable with `options(blueprintr.warn_empty_blueprints_dirs = TRUE)`

# blueprintr 0.2.2
* Adds `.SOURCE("table_name")` and `mark_source(obj)` to add variable (and table) lineage to non-blueprint-created tables. Particularly useful for including data fetched from the web into lineage
* Fixes lineage legends to focus on object type, rather than grouping. In large projects, the table groups would make the legend explode with the number of labels, rendering it useless.

# blueprintr 0.2.1
* Implements variable lineage through `options(blueprintr.use_variable_uuids = TRUE)`. Visualize variable lineage with `blueprintr::vis_variable_lineage` or inspect specific parts of the graph with an igraph generated with `blueprintr::load_variable_lineage`.
* Visualize blueprint table lineage with `blueprintr::vis_table_lineage` and inspect specific parts of the graph with `blueprintr::load_table_lineage`.
* Implements improved annotations through an option `blueprintr.use_improved_annotations`. Metadata now always overwrites annotations in the output dataset, but `mutate_annotation()` and `mutate_annotation_across()` will override the metadata safely. No longer a need to mess with `annotate_mutate`!

# blueprintr 0.2.0
* Adds ability to run macro statements in interactive mode. Very useful when debugging pipelines! Set the `blueprintr.interactive_eval_macros` option to `TRUE` to enable.
* Adds ability to set `metadata_file_path` based on the directory and name of the affiliated blueprint script. For example, a blueprint defined in "blueprints/ex/test.R" will get its default metadata file location set to "blueprints/ex/test.csv". Set the `blueprintr.use_local_metadata_path` option to `TRUE` to enable.
* Fixes a bug where targets patterns were being evaluated prematurely

# blueprintr 0.1.3
* Fixes an issue where qualified expressions like "package::thing" were not being translated correctly

# blueprintr 0.1.2
* Fixes double-sided formulae rendering in blueprints
* Exports `bpstep()` and `bp_add_step()` for customization
* Allows argument forwarding to `kfa::kfa()` using the `kfa_args` parameter in `bp_export_kfa_report()`

# blueprintr 0.1.1
* Clarifies which coding string cannot be evaluated during codebook generation or labelling
* Fixes a bug where `bp_extend()` would clobber the results of `bp_add_bpstep()` 

# blueprintr 0.1.0
* Adds capability to define custom pipeline steps, executed after completion of the `_final` stage
* Moves codebook export step to new custom step
* Adds [kfa](https://github.com/knickodem/kfa) report generation step capability

# blueprintr 0.0.11
* Uses UTF-8 encoding when parsing blueprint files (#41)
* Ignores private (dot-prefixed) metadata fields during variable annotation (#40)

# blueprintr 0.0.10

* Adds a `.with_names` parameter to `mutate_annotation_across` that, when `TRUE`, sends a column and its name as arguments to `.fn`.

# blueprintr 0.0.9

* Bugfix to ensure the "description" field is present in the event that parent, annotated datasets don't have the "description" field filled in at all
* Bugfix that didn't specify a join column, causing nuisance noise

# blueprintr 0.0.8

* Adds "annotations", a variable attribute system that makes it easier to propagate metadata provenance, rather than depending on variable naming from `.TARGET()` calls.
* Adds `mutate_annotation()` and `mutate_annotation_across()` which allows runtime transformation of metadata. Useful for batch transformations before metadata creation.

# blueprintr 0.0.7

* Adds `recurse` parameter to `load_blueprints()` and `tar_blueprints()` to recursively load blueprints from the provided `directory`

# blueprintr 0.0.6

* Integration with `panelcleaner` to import homogenized codings if target data.frame is a `mapped_df`
* Adds a new `bp_include_panelcleaner_meta()` extension to signal blueprintr to add `mapped_df` meta

# blueprintr 0.0.5

* Fixed bug that would not translate anonymous functions and formulae correctly (#22 and #29)

# blueprintr 0.0.4

* Add targets support, with `tar_blueprint()` and `tar_blueprints()`
* Change `create_metadata_file()` to return the metadata file path. To compensate, a new build step `*_meta_path` has been added that removes the metadata logic inconsistency. Target topology should be much cleaner and clearer.
* Added capability to group variables together for shared descriptions in codebooks with `group` field
* Add `file` parameter for `bp_export_codebook()` to access `codebook_file` parameter in blueprint

# blueprintr 0.0.3

* Added blueprint extension functions `bp_export_codebook()` and `bp_label_variables()` that create a pipe-able method of setting blueprint configuration.

# blueprintr 0.0.2

* Created codebook export feature with `render_codebook()` (#3)
* Allow labelled dataset generation with `blueprint(..., labelled = TRUE)`. Handy to have when making datasets intended to be used in a STATA or SPSS environment.
* Load blueprints from file or folder using `load_blueprint()` or `load_blueprints()` (#10)
