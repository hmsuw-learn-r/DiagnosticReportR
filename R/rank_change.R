# My notes to be removed:

# Need to check how function handles two groups with same value used for ranking
# Write tests and use

test_old <- get_mort_outputs("birth sex ratio", "estimate", gbd_year = 2019)
test_new <- get_mort_outputs("birth sex ratio", "estimate", run_id = 72)

# ------------------------------------------------------------------------------

#' rank_change
#'
#' Make comparisons between two versions of the same outcome by some grouping
#' variables. rank_change can make level (i.e. percentage change) and 
#' trend (i.e. linear trend) comparisons. Level changes are ranked by the 
#' greatest mean absolute percentage change to the smallest. Trend changes are 
#' ranked first by whether or not the slope changed sign and then by the 
#' greatest mean absolute percentage change to the smallest.
#'
#' @param new_data `data.table()` New version of data for comparison.
#' @param old_data `data.table()` Old version of data for comparison.
#' @param change_type `character()` Type of comparison to be made. Must be one 
#' of "level" or "trend".
#' @param threshold `numeric()` The value of threshold depends on  
#' change_type. For level changes, threshold must be between 0 and 100
#' (inclusive). It represents the maximum absolute percent difference of 
#' comparison_var by group_vars to be included in the analysis. For trend
#' changes, threshold represents the number of bins to use in the trend 
#' analysis within each grouping by group_vars. The default threshold is 
#' 1, which would examine the change in the slope (i.e. change in comparison_var
#'  over change in trend_var) by group_vars in the trend analysis, and would 
#'  examine changes greater than 1% by group_vars in the level analysis.
#' @param comparison_var `character()` Outcome variable to make comparisons 
#' over.
#' @param id_vars `character()` Vector of variable names that uniquely identify
#' both new_data and old_data.
#' @param group_vars `character()` Level to summarize change variables for
#' ranking. group_vars are a subset of id_vars.
#' @param trend_var `character()` Predictor variable used to examine trend 
#' changes. trend_var is a subset of id_vars of length 1.
#'
#' @return `data.table()` of ranked merged data.tables, which may be a subset 
#' of the merged data.tables. Rank number 1 corresponds to the greatest change
#' in comparison_var by group_vars between new_data and old_data, rank
#' number 2 corresponds to the second greatest change in comparison_var by
#' group_vars, etc.
#' 
#' @import data.table
#' @import assertable
#'
#' @export
#'
#' @examples
#' rank_change()
#'
#' \dontrun{
#' rank_change()
#' }

rank_changes <- function(new_data,
                         old_data,
                         change_type,
                         threshold = 1,
                         comparison_var,
                         id_vars,
                         group_vars,
                         trend_var = NULL) {
  
  # Check inputs ---------------------------------------------------------------
  
  for(dt in c("new_data", "old_data"))
  {
    data <- get(dt)
    assert_values(data, names(data), "not_na")
    assert_colnames(data, c(id_vars, comparison_var))
    if(sum(duplicated(data[, ..id_vars]))) != 0) {
      stop(paste("id_vars does not uniquely identify", dt))
    }
  }
  
  if(!change_type %in% c("level", "trend")) {
    stop("change_type must be one of: level, trend")
  }
  
  if(is.null(group_vars)) stop("group_vars cannot be null.")
  
  if(!is.null(setdiff(group_vars, id_vars))) {
    stop("group_vars must be a subset of id_vars.")
  }
  
  if(change_type == "level"){
    
    if(!is.null(trend_var)) stop("Examining level. trend_var must be null.")
    
    if(!is.null(setdiff(trend_var, id_vars))) {
      stop("trend_var must be one of id_vars.")
    }
    
    if(threshold < 0 | threshold > 100) {
      stop("threshold must be between 0 and 100.")
    }
  }
  
  if(change_type == "trend"){
    
    if(length(trend_var) != 1){
      stop("Trend changes can only be assessed by one variable.")
    }
    
    if(threshold < 1) stop("threshold must be greater than or equal to 1.")
    
    if!(is.integer(threshold)) {
      stop("threshold must be integer for trend analysis.")
    }
    
  }
  
  # Warnings
  if(nrow(new_data) != nrow(old_data)) {
    warning("new_data and old_data do not have the same number of rows. \n
            Comparisons will only be made by common id_vars")
  }
  
  # Combine data ---------------------------------------------------------------
  
  setnames(new_data, comparison_var, paste0("new_", comparison_var))
  new_comp <- paste0("new_", comparison_var)
  setnames(old_data, comparison_var, paste0("old_", comparison_var))
  old_comp <- paste0("old_", comparison_var)
  
  change_data <- merge(new_data, old_data, by = id_vars)
  
  if(nrow(change_data) == 0) stop("Merged data has zero rows. Check inputs.")
  
  # Assess trend ---------------------------------------------------------------
  
  if(change_type == "trend"){
    
    if(change_sig != 1){
      
      change_data[, bin := (trend_var, change_sig, labels = 1:change_sig),
                  by = group_vars]
      
    } else {
      
      change_data[, bin := 1]
    }
    
    change_data[, old_slope := coef(lm(old_comp ~ trend_var))[2], 
                by = c(group_vars, bin)]
    
    change_data[, new_slope := coef(lm(new_comp ~ trend_var))[2], 
                by = c(group_vars, bin)]
    
    change_data[, sign_change := 0]
    change_data[(old_slope < 0 & new_slope > 0) | (old_slope > 0 & new_slope < 0),
                sign_change := 1]
    
  }
  
  # Calculate percentage change ------------------------------------------------
  
  if(change_type == "level"){
    
    new_var <- new_comp
    old_var <- old_comp
    
  } else {
    
    new_var <- "new_slope"
    old_var <- "old_slope"
  }
  
  change_data[, pert_diff := 
                (get(new_var) - get(old_var)) / abs(get(old_var)) * 100]
  
  # Rank ---------------------------------------------------------------------
  
  if(change_type == "level") {
    
    change_data[, max_abs_pert_diff := max(abs(pert_diff)), by = group_vars]
    
    change_data <- change_data[max_abs_pert_diff >= threshold]
    
    if(nrow(change_data) == 0) {
      
      message(paste("There are no level changes at threshold:" threshold, "%"))
    }
    
    change_data[, mean_abs_pert_diff := mean(abs(pert_diff)), by = group_vars]
    
    change_data[order(mean_abs_pert_diff)]
    
    change_data[, rank := 1:.N, by = group_vars]
    
  } else {
    
    change_data[, mean_abs_pert_diff := mean(abs(pert_diff)), 
                by = c(group_vars, bin)]
    
    change_data[order(sign_change, mean_abs_pert_diff)]
    
    change_data[, rank := 1:.N, by = c(group_vars, bin)] 
    
  }
  
  # Final output ---------------------------------------------------------------
  
  return(change_data)
}