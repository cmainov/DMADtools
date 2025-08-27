# DMADtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/cmainov/DMADtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmainov/DMADtools/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/cmainov/DMADtools/graph/badge.svg)](https://app.codecov.io/gh/cmainov/DMADtools)
#' `r lifecycle::badge('experimental')`
<!-- badges: end -->
  
Functions and data analysis tools.

### Description
___

The functions in this package can be used to facilitate data analysis in a public health context.

* `summary_table`: *Generate a "Nice" `flextable`, cross-tabulating up to three variables.*

* `dc_mapr`: *Create a "nice" publication-level map of Washington, D.C. by ward (2012 or 2022 redistricting), Zip-Code Tabulation Area (ZCTA), Census tract, or block.*

* Geocoding tools, `validate_address_dc` and `mar2_find`: *wrapper functions for `tidymar::find_location()` that queries the DC’s Master Address Repository (MAR).*
### Install
___

Install the development version hosted on this repository (requires the `devtools` package) by specifying the following syntax:

```
# install.packages( "devtools" )
devtools::install_github( "cmainov/DMADtools" )
```

### Bug-reporting and Contributions
___

**DMADtools** is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/gpl-3.0.txt). You can report any bugs, ask questions, or suggest new features in the [issue queue](https://github.com/cmainov/DMADtools/issues). Pull requests and contributions will be reviewed and incorporated into the package at the discretion of the package maintainer.

___
