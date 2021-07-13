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
