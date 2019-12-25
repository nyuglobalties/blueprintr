---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# blueprintr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/blueprintr)](https://CRAN.R-project.org/package=blueprintr)
<!-- badges: end -->

Over the course of a research project, many datasets could be used or generated, and sometimes it's tricky to effectively document each dataset with its own data dictionary and to comprehensively trace data lineage. With [`drake`](https://github.com/ropensci/drake), the data lineage part can be done manually, but with many datasets it can be cumbersome to define a target for each part in the data transformation process. `blueprintr` creates a project structure that builds into or modifies existing `drake` plans automatically.


## Installation

You can install the released version of blueprintr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("blueprintr")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(blueprintr)
#> Error in library(blueprintr): there is no package called 'blueprintr'
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


```r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" title="plot of chunk pressure" alt="plot of chunk pressure" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!

## Contributing

Please note that the 'blueprintr' project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
