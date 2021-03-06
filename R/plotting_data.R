#' Old and new outcome variable measured over time for different locations in the U.S.
#'
#' An example of output data from rank_change function for trend analysis. This data is
#' a subset of a larger output file. The variables are as follows:
#'
#' @format A data frame with 121 rows and 5 columns
#' \describe{
#'   \item{ihme_loc_id}{Location identifier, three letter country code plus state code}
#'   \item{year_id}{Year identifier for the estimates}
#'   \item{new_mean}{The newer estimate}
#'   \item{old_mean}{The older estimate}
#'   \item{rank}{The rank of each location, determined by rank_change}
#' }
"trend_changes_USA"

