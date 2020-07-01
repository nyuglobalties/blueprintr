
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blueprintr <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/blueprintr)](https://CRAN.R-project.org/package=blueprintr)
[![R build
status](https://github.com/Global-TIES-for-Children/blueprintr/workflows/R-CMD-check/badge.svg)](https://github.com/Global-TIES-for-Children/blueprintr/actions)
<!-- badges: end -->

Over the course of a research project, many datasets could be used or
generated, and sometimes it’s tricky to effectively document each
dataset with its own data dictionary and to comprehensively trace data
lineage. With [`drake`](https://github.com/ropensci/drake), the data
lineage part can be done manually, but with many datasets it can be
cumbersome to define a target for each part in the data transformation
process. `blueprintr` creates a project structure that builds into or
modifies existing `drake` plans automatically.

## Installation

As `blueprintr` is not yet on CRAN, you must install the package from
this repository:

``` r
install.packages("remotes")
remotes::install_github("Global-TIES-for-Children/blueprintr")
```

## Contributing

Please note that the ‘blueprintr’ project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
