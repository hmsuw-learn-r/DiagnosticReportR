#' An old outcome variable measured over time for different groups
#'
#' An example data set with an outcome and a grouping variable. The variables
#'  are as follows:
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{outcome}{An outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#' }
"old_data"

#' Another old outcome variable measured over time for different groups
#'
#' An example data set with an outcome and a grouping variable. The only
#' difference between old_data nd old_data_alt is the outcome variable.
#' The variables are as follows:
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{outcome}{An outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#' }
"old_data_alt"

#' A new outcome variable measured over time for different groups.
#'
#' An example data set with an outcome and a grouping variable. Measured using
#' a different method and/or different input data. The variables are as follows:
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{outcome}{An outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#' }
"new_data"

#' Another new outcome variable measured over time for different groups.
#'
#' An example data set with an outcome and a grouping variable. Measured using
#' a different method and/or different input data. The only
#' differences between new_data and new_data_alt are the number of groups and
#' the outcomes. The variables are as follows:
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{outcome}{An outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#' }
"new_data_alt"

#' The expected level analysis
#'
#' The expected level analysis associated with the test on old_data and
#' new_data by the grouping variable. See the tests for input arguements.
#' The variables are as follows:
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{old_outcome}{The original outcome of interest}
#'   \item{new_outcome}{The revised outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#'   \item{pert_diff}{The percentage difference between new_outcome and
#'   old_outcome}
#'   \item{max_abs_pert_diff}{The maximum absolute percentage difference in
#'   each group}
#'   \item{mean_abs_pert_diff}{The mean absolute percentage difference in each
#'   group}
#'   \item{rank}{The ranking of the differences in the outcome variable between
#'    the new and old data where 1 is the greatest mean_abs_pert_diff.}
#' }
"expected_level"

#' The expected trend analysis
#'
#' The expected trend analysis associated with the test on old_data and
#' new_data by the grouping variable. See the tests for input arguements.
#' The variables are as follows:
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{old_outcome}{The original outcome of interest}
#'   \item{new_outcome}{The revised outcome of interest}
#'   \item{year}{Year the data was "collected"}
#'   \item{group}{The group associated with the outcome}
#'   \item{bin}{The sub-group based on the trend_var within each group}
#'   \item{old_slope}{The rate of change in old_outcome by the trend_var within
#'   each sub-group}
#'   \item{new_slope}{The rate of change in new_outcome by the trend_var within
#'   each sub-group}
#'   \item{sign_change}{Dummy variable where 1 indicates a sign change in the
#'   slope between the old and new data within each sub-group}
#'   \item{numb_sign_change}{The number of bins with a sign change in each
#'   group}
#'   \item{pert_diff}{The percentage difference between new_outcome and
#'   old_outcome}
#'   \item{mean_abs_pert_diff}{The mean absolute percentage difference in each
#'   group}
#'   \item{rank}{The ranking of the differences in the outcome variable between
#'    the new and old data where 1 is the greatest numb_sign_change and
#'    mean_abs_pert_diff.}
#' }
"expected_trend"
