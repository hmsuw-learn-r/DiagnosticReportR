# Testing plots to review/compare overall changes (via scatters)

rm(list=ls())

library(ggplot2)

# Test data: road injury incidence estimate comparison data (country level, compare GBD 2019 and GBD 2020 best)
road_inj_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc.csv"))

# Test data: results from rank_changes function, for birth sex ratios
level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))
trend_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/trend_changes.csv"))

#' plot_scatter
#' 
#' This function can be used to visualize the results of the rank_change function, or to otherwise compare values from two 
#' different model versions or sets of estimates. This can help streamline the vetting process. All characteristics for each estimate 
#' should be matched, e.g. year, location, age. Plot_scatter creates scatter plots comparing the two sets of results. The various
#' options for faceting or coloring the points allow the user to customize the plot to maximize utility. 
#'
#'
#' @param data data.frame or data.table. Data to plot comparing two sets of estimates, e.g. output of rank_change function. Required!
#' @param x_var Character. Variable to plot on the x-axis, e.g the "old" version or previous estimate. Required!
#' @param y_var Character. Variable to plot on the y-axis, e.g. the "new" version or updated estimate. Required!
#' @param facet_x Character. Variable to facet by, along x-axis. This is required in order to use facet_wrap and facet_grid. Defaults to NULL.
#' @param facet_y Character. Variable to facet by, along y-axis, if desired. This is required in order to use facet_grid. Defaults to NULL.
#' @param color_pt Character. Variable to modify color of geom_point, if desired. Defaults to NULL.
#' @param title Character. Title for plot, if desired. Defaults to NULL.
#' @param facet_type Character. Choice of "none", "wrap", or "grid". Defaults to "none". The "none" option will create a single plot. 
#' The "wrap" option will produce a plot using facet_wrap. The "grid" option will produce a plot using facet_grid. Required!
#'
#' @return A ggplot object
#' 
#' @import ggplot2
#' 
#' @export
#'
#' @examples 
#' plot_scatter(data = changes_df, x_var = "old_mean", y_var = "new_mean", facet_x = "ihme_loc_id", color_pt = "year_id", facet_type = "wrap")
#' plot_scatter(data = changes_df, x_var = "old_mean", y_var = "new_mean, facet_x = "ihme_loc_id", facet_y = "sex_id, facet_type = "grid")
plot_scatter <- function(data, 
                         x_var, 
                         y_var, 
                         facet_x, 
                         facet_y = NULL, 
                         color_pt = NULL,
                         title = NULL,
                         facet_type = "none"){
  
  
  if (facet_type == "none"){
    
    ggplot(data) + 
      geom_point(aes_string(x = x_val, y = y_val, color = color_pt)) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title)
      
  }
  
  else if (facet_type == "wrap" & is.character(facet_x)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(data) +
      geom_point(aes_string(x = x_val, y = y_val, color = color_pt)) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else if (facet_type == "grid" & is.character(facet_x) & is.character(facet_y)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(data) +
      geom_point(aes_string(x = x_val, y = y_val, color = color_pt)) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") +
      facet_grid(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else {
    stop("Are all variable names character strings? Did you specify your facet_type correctly, based on facet_x and facet_y?")
  }
  
}


