#' plot_scatter
#' 
#' This function can be used to visualize the results of the rank_change function, or to otherwise 
#' compare values from two different model versions or sets of estimates. This can help streamline 
#' the vetting process. All characteristics for each estimate should be matched, e.g. year, 
#' location, age. Plot_scatter creates scatter plots comparing the two sets of results. The 
#' various options for faceting or coloring the points allow the user to customize the plot to 
#' maximize utility. 
#'
#' @param data data.frame or data.table. Data to plot comparing two sets of estimates, e.g. 
#' output of rank_change function. Required!
#' @param x_var `character()` Variable to plot on the x-axis, e.g the "old" version or 
#' previous estimate. Required!
#' @param y_var `character()` Variable to plot on the y-axis, e.g. the "new" version or 
#' updated estimate. Required!
#' @param facet_x `character()` Variable to facet by, along x-axis. This is required in order
#' to use facet_wrap and facet_grid. Defaults to NULL.
#' @param facet_y `character()` Variable to facet by, along y-axis, if desired. This is required 
#' in order to use facet_grid. Defaults to NULL.
#' @param color_pt `character()` Variable to modify color of geom_point, if desired. 
#' Defaults to NULL.
#' @param pt_size `numeric()` Option to modify point size. Defaults to 1. 
#' @param title `character()` Title for plot, if desired. Defaults to NULL.
#' @param facet_type `character()` Choice of "none", "wrap", or "grid". Defaults to "none". The
#' "none" option will create a single plot. The "wrap" option will produce a plot using facet_wrap. 
#' The "grid" option will produce a plot using facet_grid. 
#' @param fix_scale `boolean()` Option to fix the axes for plot, using the min and max values. 
#' Defaults to FALSE. Will override scales = "free".
#'
#' @return A ggplot object
#' 
#' @import ggplot2
#' 
#' @export
#'
#' @examples 
#' plot_scatter(trend_changes_USA, 
#'              x_var = "old_mean", 
#'              y_var = "new_mean", 
#'              facet_x = "ihme_loc_id", 
#'              pt_size = 2, 
#'              facet_type = "wrap", 
#'              fix_scale = TRUE, 
#'              color_pt = "year_id")

plot_scatter <- function(data, 
                         x_var, 
                         y_var, 
                         facet_x, 
                         facet_y = NULL, 
                         color_pt = NULL,
                         pt_size = 1,
                         title = NULL,
                         facet_type = "none",
                         fix_scale = FALSE){
  
  data <- as.data.table(data)
  
  if (facet_type == "none"){
    
    my_scatter <- ggplot(data) + 
      geom_point(aes_string(x = x_var, y = y_var, color = color_pt), size = pt_size, alpha = 0.7) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") + 
      ggtitle(title)
      
  }
  
  else if (facet_type == "wrap" & is.character(facet_x)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    my_scatter <- ggplot(data) + 
      geom_point(aes_string(x = x_var, y = y_var, color = color_pt), size = pt_size, alpha = 0.7) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else if (facet_type == "grid" & is.character(facet_x) & is.character(facet_y)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    my_scatter <- ggplot(data) +
      geom_point(aes_string(x = x_var, y = y_var, color = color_pt), size = pt_size, alpha = 0.7) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") +
      facet_grid(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else {
    stop("Are all variable names character strings? 
         Did you specify your facet_type correctly, based on facet_x and facet_y?")
  }
  
  if (fix_scale == TRUE){
    my_scatter <- my_scatter + 
      scale_x_continuous(limits = c(min(data[, ..x_var], data[, ..y_var]), 
                                    max(data[, ..x_var], data[, ..y_var]))) +
      scale_y_continuous(limits = c(min(data[, ..x_var], data[, ..y_var]), 
                                    max(data[, ..x_var], data[, ..y_var]))) 
  }
  
  return(my_scatter)
}
