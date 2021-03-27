# blueprintr (development version)

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
