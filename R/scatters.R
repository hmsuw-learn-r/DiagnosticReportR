# Testing plots to review/compare overall changes (via scatters) from level changes

rm(list=ls())

library(ggplot2)

# Test data: road injury incidence estimate comparisons
age_std_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_age_std.csv"))
all_age_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_ages_year_2019.csv"))

# Test data: results from rank_changes function, for birth sex ratios
level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))
trend_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/trend_changes.csv"))

# Scatter results from model 1 to model 2 (e.g. old to new)
# Create facet grid
# Option to color points by a variable
# Option to include a title
plot_scatter <- function(data, 
                         x_var, 
                         y_var, 
                         facet_x, 
                         facet_y = NULL, 
                         color_pt = NULL,
                         title = NULL,
                         facet_type = "none"){
  
  
  if (facet_type == "none" & is.null(facet_x) & is.null(facet_y)){
    
    ggplot(data) + 
      geom_point(aes_string(x = x_val, y = y_val, color = color_pt)) + 
      geom_abline(slope = 1, size = 0.5, color = "grey") + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title)
      
  }
  
  else if (facet_type == "wrap" & is.character(facet_x) & is.null(facet_y)){
    
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
