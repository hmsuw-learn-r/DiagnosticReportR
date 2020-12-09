# Testing plots to review/compare time trends 

rm(list=ls())

library(ggplot2)

# Test road injury incidence estimate comparison data
age_std_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_age_std.csv"))
all_age_inc <- as.data.table(read.csv("/ihme/scratch/users/mobergm/road_inj_inc_ages_year_2019.csv"))

# Test results from rank_changes function for birth sex ratio estimates
level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))
trend_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/trend_changes.csv"))

# Scatter results from model 1 to model 2 (e.g. old to new)
# Create facet grid
# Option to color points by a variable
# Option to include a title
facet_trends <- function(data, 
                         year_var,
                         y1_var, 
                         y2_var,
                         facet_x, 
                         facet_y = NULL, 
                         title = NULL,
                         facet_type = "wrap"){
  
  facet_formula <- as.formula(paste(facet_y, facet_x, sep = " ~ "))
  
  longer <- tidyr::pivot_longer(data, 
                                cols = c(y1_var, y2_var),
                                names_to = "version",
                                values_to = "estimate")
  
  if (facet_type == "wrap" & is.character(facet_x) & is.null(facet_y)){
    ggplot(longer) +
      geom_line(aes_string(x = year_var, y = "estimate", color = "version")) + 
      facet_wrap(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else if (facet_type == "grid" & is.character(facet_x) & is.character(facet_y)){
    ggplot(data) +
      geom_line(aes_string(x = year_var, y = "estimate", color = "version")) + 
      facet_grid(facet_formula, scales = "free") + 
      ggtitle(title)
  }
  
  else {
    stop("Are all variable names character strings? 
          Did you specify your facet_type correctly, based on facet_x and facet_y?")
  }
  
}

# Plot level changes, facet by location
ggplot(level_changes[rank <= 5], aes(x = year_id)) +
  geom_line(aes(y = old_mean, color = "previous"), size = 1) +
  geom_line(aes(y = new_mean, color = "new"), size = 1) +
  labs(color = "Model version") +
  xlab("year") +
  ylab("birth sex ratio") +
  scale_color_manual(values = c("previous" = "dodgerblue", "new" = "deeppink4")) +
  guides(color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ihme_loc_id, scales = "free") +
  ggtitle("Locations with greatest % difference in level change analysis")

# Plot top 5 trend changes, facet by location
ggplot(trend_changes[rank <= 5], aes(x = year_id)) +
  geom_line(aes(y = old_mean, color = "old"), size = 1) +
  geom_line(aes(y = new_mean, color = "new"), size = 1) +
  labs(color = "Model version") +
  xlab("year") +
  ylab("birth sex ratio") +
  facet_wrap(~ihme_loc_id, scales = "free") +
  scale_color_manual(name = "Results version", 
                     values = c("old" = "dodgerblue", "new" = "deeppink4")) + 
  ggtitle("Locations with greatest % difference in trend/slope change analysis")

# # Test highlight arbitrary section of plot
# ggplot(level_changes[ihme_loc_id == "BWA"], aes(x = year_id)) +
#   geom_line(aes(y = old_mean, color = "previous"), size = 1) +
#   geom_line(aes(y = new_mean, color = "new"), size = 1) +
#   labs(color = "Model version") +
#   xlab("year") +
#   ylab("birth sex ratio") +
#   scale_color_manual(values = c("previous" = "dodgerblue", "new" = "deeppink4")) +
#   guides(color = guide_legend(reverse = TRUE)) +
#   annotate("rect", xmin=1983, xmax=2000, ymin=1, ymax=Inf, alpha=0.2, fill="red")


