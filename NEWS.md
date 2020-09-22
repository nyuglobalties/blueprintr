# Development version (0.0.3.9000)

* Added blueprint extension functions `bp_export_codebook()` and `bp_label_variables()` that create a pipe-able method of setting blueprint configuration.

# blueprintr 0.0.2

* Created codebook export feature with `render_codebook()` (#3)
* Allow labelled dataset generation with `blueprint(..., labelled = TRUE)`. Handy to have when making datasets intended to be used in a STATA or SPSS environment.
* Load blueprints from file or folder using `load_blueprint()` or `load_blueprints()` (#10)
