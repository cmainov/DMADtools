#' Test data for examples in the `summary_table` function.
#'
#' @format `d.example`
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
"d.example"

#' Test data for examples in the `summary_table` function.
#'
#' @format `d.example.na`
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
"d.example.na"

#' Test data for examples in the `dc_mapr` function.
#'
#' @format `d.obs`
#' \describe{
#' A data frame with 45 rows and 19 columns:
#'   \item{id}{An integer. The unique id}.
#'   \item{ward}{A character variable. The DC ward. Eight levels (Ward 1:8)}
#'   \item{age}{A numeric variable. The age of the observation.}
#'   \item{cat}{A character variable. Three levels (Cat 1:3)}
#'   \item{bin}{A character variable. Two levels ("Binary 1" "Binary 2")}
#'   \item{bin_other}{An integer variable. Two levels (1, 0)}
#'   \item{ward_pop}{An integer variable. The ward population count.}
#' }
#'
"d.obs"