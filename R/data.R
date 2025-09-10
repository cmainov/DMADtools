#' Test data for examples in the `summary_table` function.
#'
#' @format `d_example`
#' \describe{
#' A data frame with 45 rows and 19 columns:
#'   \item{v1}{A character variable. Three levels (Geo 1, Geo 2, Geo 3)}
#'   \item{v2}{A character variable. Three levels (Char 1, Char 2, Char 3)}
#'   \item{v3}{A character variable. Three levels (Other Char 1, Other Char 2, Other Char 3)}
#'   \item{v4}{A character variable. Two levels (Y, N)}
#'   \item{v5}{A character variable. Two levels (Y, N)}
#'   \item{v_pop}{An integer variable. The example population count.}
#' }
#'
"d_example"

#' Test data for examples in the `summary_table` function.
#'
#' @format `d_example_na`
#' \describe{
#' A data frame with 45 rows and 19 columns:
#'   \item{v1}{A character variable. Three levels (Geo 1, Geo 2, Geo 3)}
#'   \item{v2}{A character variable. Three levels (Char 1, Char 2, Char 3)}
#'   \item{v3}{A character variable. Three levels (Other Char 1, Other Char 2, Other Char 3)}
#'   \item{v4}{A character variable. Two levels (Y, N)}
#'   \item{v5}{A character variable. Two levels (Y, N)}
#'   \item{v_pop}{An integer variable. The example population count.}
#' }
#'
"d_example_na"

#'  Ward Test data for examples in the `dc_mapr` function.
#'
#' @format `d_ward`
#' \describe{
#' A data frame with 500 rows and 7 columns:
#'   \item{id}{An integer. The unique id}.
#'   \item{ward}{A character variable. The DC ward_ Eight levels (Ward 1:8)}
#'   \item{age}{A numeric variable. The age of the observation.}
#'   \item{cat}{A character variable. Three levels (Cat 1:3)}
#'   \item{bin}{A character variable. Two levels ("Binary 1" "Binary 2")}
#'   \item{bin_other}{An integer variable. Two levels (1, 0)}
#'   \item{ward_pop}{An integer variable. The ward population count.}
#' }
#'
"d_ward"

#'  ZCTA Test data for examples in the `dc_mapr` function.
#'
#' @format `d_zcta`
#' \describe{
#' A data frame with 5000 rows and 7 columns:
#'   \item{id}{An integer. The unique id}.
#'   \item{zcta}{A character variable. The DC ZIP code tabulation area code.}
#'   \item{age}{A numeric variable. The age of the observation.}
#'   \item{cat}{A character variable. Three levels (Cat 1:3)}
#'   \item{bin}{A character variable. Two levels ("Binary 1" "Binary 2")}
#'   \item{bin_other}{An integer variable. Two levels (1, 0)}
#'   \item{zcta_pop}{An integer variable. The ZCTA population count.}
#' }
#'
"d_zcta"