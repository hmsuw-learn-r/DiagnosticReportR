# Testing plots to review/compare time trends 

rm(list=ls())

library(ggplot2)

# Test road injury incidence estimate comparison data
age_std_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_age_std.csv"))
all_age_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_ages_year_2019.csv"))

# Test results from rank_changes function for birth sex ratio estimates
level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))
trend_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/trend_changes.csv"))

# Compare results from model 1 to model 2 by year (time trends)
# Facet wrap or facet grid
# Option to include a title
# Option to choose colors
plot_trends <- function(data, 
                        year_var,
                        y1_var, 
                        y2_var,
                        facet_x = NULL, 
                        facet_y = NULL, 
                        title = NULL,
                        facet_type = "none",
                        colors = c("dodgerblue", "deeppink4")){
  
  longer <- tidyr::pivot_longer(data, 
                                cols = c(y1_var, y2_var),
                                names_to = "version",
                                values_to = "estimate")
  
  if (facet_type == "none" & is.null(facet_x) & is.null(facet_y)){
    ggplot(longer) + 
      geom_line(aes_string(x = year_var, y = "estimate", color = "version"), size = 1) + 
      geom_point(aes_string(x = year_var, y = "estimate", color = "version"), size = 2, alpha = 0.5) + 
      ggtitle(title) + 
      scale_color_manual(values = colors)
  }
  
  else if (facet_type == "wrap" & is.character(facet_x) & is.null(facet_y)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(longer) +
      geom_line(aes_string(x = year_var, y = "estimate", color = "version"), size = 1) + 
      geom_point(aes_string(x = year_var, y = "estimate", color = "version"), size = 2, alpha = 0.5) + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title) +
      scale_color_manual(values = colors)
  }
  
  else if (facet_type == "grid" & is.character(facet_x) & is.character(facet_y)){
    
    facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
    
    ggplot(longer) +
      geom_line(aes_string(x = year_var, y = "estimate", color = "version"), size = 1) + 
      geom_point(aes_string(x = year_var, y = "estimate", color = "version"), size = 2, alpha = 0.5) + 
      facet_grid(facet_formula, scales = "free") + 
      scale_color_manual(values = colors)
      ggtitle(title)
  }
  
  else {
    stop("Are all variable names character strings? Did you specify your facet_type correctly, based on facet_x and facet_y?")
  }
  
}

