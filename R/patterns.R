#' plot_trends
#' 
#' This function can be used to visualize the results of the rank_change function, or to otherwise 
#' compare values from two different model versions or sets of estimates. This can help streamline 
#' the vetting process. All characteristics for each estimate should be matched, e.g. year, location, 
#' age. plot_trends creates plots of the estimates over some unit of time comparing the two sets
#' of results. This could be used for time trends across years, or age patterns. The options for faceting 
#' or coloring allow the user to customize the plot to maximize utility. This function assumes that the
#' two sets of estimates are in two different columns, e.g. old_mean and new_mean, and uses pivot_longer
#' from tidyr to reshape the data long to make plotting more straightforward and flexible. 
#'
#' @param data data.table or data.frame. Data to plot comparing two sets of estimates, e.g. 
#' output of rank_change function. Required!
#' @param time_var `character()` Variable name for time, e.g. year/year_id or ages. Required!
#' @param y1_var `character()` Variable name for first set of results/estimates, e.g. the previous or
#'  old set for comparison. Required!
#' @param y2_var `character()` Variable name for second set of results/estimates, e.g. the updated 
#' or new set for comparison. Required!
#' @param facet_x `character()` Variable to facet by, along x-axis. This is required in order to use 
#' facet_wrap and facet_grid. Defaults to NULL.
#' @param facet_y `character()` Variable to facet by, along y-axis, if desired. This is required in 
#' order to use facet_grid. Defaults to NULL.
#' @param title `character()` Title of plot, if desired. Defaults to NULL.
#' @param facet_type `character()` Choice of "none", "wrap", or "grid". Defaults to "none". The "none"
#' option will create a single plot. The "wrap" option will produce a plot using facet_wrap. The "grid" 
#' option will produce a plot using facet_grid. Required!
#' @param colors Vector. This vector would contain the name of two colors to be used in the plot 
#' to differentiate the two versions or sets of values. This defaults to dodgerblue and deeppink4, 
#' but can be otherwise specified by the user.
#' @param my_labs Vector. This vector would contain the labels/names you want to use for the two different sets of 
#' estimates. If left blank this will default to the column names for y1_var and y2_var. 
#' @param line_size `numeric()` Can select line thickness. Default is 1. 
#'
#' @return A ggplot object. 
#' 
#' @import tidyr 
#' @import ggplot
#' 
#' @export
#'
#' @examples
#' plot_trends(changes_df, time_var = "year_id", y1_var = "old_mean", y2_var = "new_mean", colors = c("blue", "red))
#' plot_trends(changes_df, time_var = "year_id", y1_var = "old_mean", y2_var = "new_mean", facet_x = "age", facet_y = "sex", facet_type = "grid")
#' plot_trends(changes_df, time_var = "age", y1_var = "old_mean", y2_var = "new_mean", facet_x = "location_id")
plot_trends <- function(data, 
                        time_var,
                        y1_var, 
                        y2_var,
                        facet_x = NULL, 
                        facet_y = NULL, 
                        title = NULL,
                        facet_type = "none",
                        colors = c("dodgerblue", "deeppink4"),
                        my_labs = c(y1_var, y2_var), 
                        line_size = 1){
  
  longer <- tidyr::pivot_longer(data, 
                                cols = c(y1_var, y2_var),
                                names_to = "version",
                                values_to = "estimate")
  
  if (facet_type == "none"){
    ggplot(longer) + 
      geom_line(aes_string(x = time_var, y = "estimate", color = "version"), size = line_size) + 
      ggtitle(title) + 
      scale_color_manual(values = colors, labels = my_labs)
  }
  
  else if (facet_type == "wrap" & is.character(facet_x)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(longer) +
      geom_line(aes_string(x = time_var, y = "estimate", color = "version"), size = line_size) + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title) +
      scale_color_manual(values = colors, labels = my_labs)
  }
  
  else if (facet_type == "grid" & is.character(facet_x) & is.character(facet_y)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(longer) +
      geom_line(aes_string(x = time_var, y = "estimate", color = "version"), size = line_size) + 
      facet_grid(facet_formula, scales = "free") + 
      scale_color_manual(values = colors, labels = my_labs) +
      ggtitle(title)
  }
  
  else {
    stop("Are all variable names character strings? Did you specify your facet_type correctly, based on facet_x and facet_y?")
  }
  
}


