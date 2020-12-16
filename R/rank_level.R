#' rank_level
#'
#' Make comparisons between two versions of the same outcome by some grouping
#' variables and rank them by the greatest mean absolute percentage
#' change to the smallest.
#'
#' @param new_data `data.table()` New version of data for comparison.
#' @param old_data `data.table()` Old version of data for comparison.
#' @param threshold `numeric()` The threshold must be greater than or equal
#' to zero. It represents the maximum absolute percent difference of
#' comparison_var by group_vars to be included in the analysis. The default is
#' zero.
#' @param comparison_var `numeric()` Outcome variable to make comparisons
#' over.
#' @param id_vars Vector of variable names that uniquely identify
#' both new_data and old_data.
#' @param group_vars Level to summarize change variables for
#' ranking. group_vars are a subset of id_vars.
#' @param displace_old_zero `numeric()` Displace zeros in old_data to prevent
#' NaNs in change calculations. Default is 1e-10.
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
#'
#' @export
#'
#' @examples
#' rank_level(new_data,
#'             old_data,
#'             threshold = 5,
#'             comparison_var = "outcome",
#'             id_vars = c("year", "group"),
#'             group_vars = "group")


rank_level <- function(new_data,
                       old_data,
                       threshold = 0,
                       comparison_var,
                       id_vars,
                       group_vars,
                       displace_old_zero = 1e-10) {

  # arguments set up -----------------------------------------------------------

  pert_diff <- max_abs_pert_diff <- mean_abs_pert_diff <- NULL

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

  if(displace_old_zero == 0) {
    stop("Cannot be 0. Specify a number to avoid potential NaNs in the output.")
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

  # Rank prep ------------------------------------------------------------------

  change_data[get(paste0("old_", comparison_var)) == 0,
              paste0("old_", comparison_var) :=
                get(paste0("old_", comparison_var)) + displace_old_zero]

  change_data[, pert_diff :=
                ((get(new_comp) - get(old_comp)) / abs(get(old_comp))) * 100]

  change_data[, max_abs_pert_diff := max(abs(pert_diff)), by = group_vars]

  change_data <- change_data[max_abs_pert_diff >= threshold]

  if(nrow(change_data) == 0) {

    message(paste("There are no level changes at threshold:", threshold, "%"))
  }

  change_data[, mean_abs_pert_diff := mean(abs(pert_diff)), by = group_vars]


  # Rank -----------------------------------------------------------------------

  change_data <- setorderv(change_data,
                           cols = c("mean_abs_pert_diff", group_vars),
                           order = -1)

  change_data[, rank := frank(-mean_abs_pert_diff, ties.method = "dense")]


  # Final output ---------------------------------------------------------------

  return(change_data)
}
