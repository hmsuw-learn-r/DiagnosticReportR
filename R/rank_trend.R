#' rank_trend
#'
#' Make comparisons between two versions of the same outcome by some grouping
#' variables where trend changes are ranked first by the number of slopes that
#' changed sign and then by the greatest mean absolute percentage change to the
#' smallest.
#'
#' @param new_data `data.table()` New version of data for comparison.
#' @param old_data `data.table()` Old version of data for comparison.
#' @param numb_bins `numeric()` numb_bins represents the number of bins to use
#' in the trend analysis within each grouping by group_vars. The default
#' numb_bins is 1, which would examine the change in the slope (i.e. change in
#' comparison_var over change in trend_var) by group_vars in the trend analysis.
#' @param comparison_var `numeric()` Outcome variable to make comparisons
#' over.
#' @param id_vars Vector of variable names that uniquely identify
#' both new_data and old_data.
#' @param group_vars Level to summarize change variables for
#' ranking. group_vars are a subset of id_vars.
#' @param trend_var `numeric()` Predictor variable used to examine trend
#' changes. trend_var is a subset of id_vars of length 1.
#' @param displace_old_zero `numeric()` Displace zeros in old_data trend to
#' prevent NaNs in change calculations. Default is 1e-10.
#'
#' @return `data.table()` of ranked merged data.tables, which may be a subset
#' of the input data.tables. Rank number 1 corresponds to the greatest change
#' in comparison_var by group_vars between new_data and old_data, rank
#' number 2 corresponds to the second greatest change in comparison_var by
#' group_vars, etc. NOTE: Ranks may be duplicated by group if they have the same
#' values used for ranking.
#'
#' @import data.table
#' @import assertable
#' @import stats
#'
#' @export
#'
#' @examples
#' rank_trend(new_data,
#'            old_data_alt,
#'            numb_bins = 1L,
#'            comparison_var = "outcome",
#'            id_vars = c("year", "group"),
#'            group_vars = "group",
#'            trend_var = "year")


rank_trend <- function(new_data,
                       old_data,
                       numb_bins = 1L,
                       comparison_var,
                       id_vars,
                       group_vars,
                       trend_var,
                       displace_old_zero = 1e-10) {

  # arguments set up -----------------------------------------------------------

  bin <- old_slope <- new_slope <- sign_change <- numb_sign_change <-  NULL
  min_group_size <- pert_diff  <- mean_abs_pert_diff <- NULL

  # Check inputs ---------------------------------------------------------------

  for(dt in c("new_data", "old_data")) {

    data <- get(dt)
    assert_values(data, names(data), "not_na", quiet = T)
    assert_colnames(data, c(id_vars, comparison_var), quiet = T)
    if(sum(duplicated(data[, id_vars, with = F])) != 0) {
      stop(paste("id_vars does not uniquely identify", dt))
    }
  }

  if(is.null(group_vars)) stop("group_vars cannot be null.")

  if(length(setdiff(group_vars, id_vars)) != 0) {
    stop("group_vars must be a subset of id_vars.")
  }

  if(length(trend_var) != 1){
    stop("Trend changes can only be assessed by one variable.")
  }

  if(length(setdiff(trend_var, id_vars)) != 0) {
    stop("trend_var must be one of id_vars.")
  }

  if(displace_old_zero == 0) {
    stop("Cannot be 0. Specify a number to avoid NaNs in the output.")
  }

  if(numb_bins < 1) {
    stop("numb_bins must be >= 1.")
  }

  if(!is.integer(numb_bins)) {

    warning("numb_bins is not an integer. Coercing.")

    numb_bins <- as.integer(numb_bins)

  }

  min_group_size <- min(c(old_data[, .N, by = group_vars]$N,
                          new_data[, .N, by = group_vars]$N))

  if((min_group_size / numb_bins) < 2) {
    stop("A regression cannot be fit with < 2 rows in a sub-group")
  }


  # Warnings
  if(nrow(new_data) != nrow(old_data)) {
    warning("new_data and old_data do not have the same number of rows. \n
            Comparisons will only be made by common id_vars")
  }

  # Combine data ---------------------------------------------------------------

  new_dt <- copy(new_data)
  old_dt <- copy(old_data)

  setnames(new_dt, comparison_var, paste0("new_", comparison_var))
  new_comp <- paste0("new_", comparison_var)
  setnames(old_dt, comparison_var, paste0("old_", comparison_var))
  old_comp <- paste0("old_", comparison_var)

  change_data <- merge(new_dt, old_dt, by = id_vars)

  if(nrow(change_data) == 0) stop("Merged data has zero rows. Check inputs.")

  # Assess trend ---------------------------------------------------------------

  if(numb_bins != 1){

    change_data[, bin := cut(get(trend_var), numb_bins, labels = 1:numb_bins),
                by = group_vars]

  } else {

    change_data[, bin := 1]
  }

  change_data[, old_slope := coef(lm(get(old_comp) ~ get(trend_var)))[2],
              by = c(group_vars, "bin")]

  change_data[, new_slope := coef(lm(get(new_comp) ~ get(trend_var)))[2],
              by = c(group_vars, "bin")]

  change_data[, sign_change := 0]
  change_data[(old_slope < 0 & new_slope > 0) | (old_slope > 0 & new_slope < 0),
              sign_change := 1]

  change_data[old_slope == 0, old_slope := old_slope + displace_old_zero]

  # Rank prep ----------------------------------------------------------------

  change_data[, pert_diff :=
                ((new_slope - old_slope) / abs(old_slope)) * 100]

  change_data[, mean_abs_pert_diff := mean(abs(pert_diff)),
              by = group_vars]

  sign_vars <- c(group_vars, "bin", "sign_change")
  sign_data <- unique(change_data[, sign_vars, with = F])
  sign_data[, numb_sign_change := sum(sign_change), by = group_vars]
  sign_data <- unique(sign_data[, c("bin", "sign_change") := NULL])

  change_data <- merge(change_data, sign_data, by = group_vars)

  # Rank ---------------------------------------------------------------------

  change_data <- setorderv(change_data,
                           cols = c("numb_sign_change", "mean_abs_pert_diff",
                                    group_vars),
                           order = -1)

  change_data[, rank := frank(list(-numb_sign_change, -mean_abs_pert_diff),
                              ties.method = "dense")]

  # Final output ---------------------------------------------------------------

  return(change_data)
}
