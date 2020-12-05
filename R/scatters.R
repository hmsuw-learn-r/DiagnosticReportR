# Testing plots to review/compare overall changes (via scatters) from level changes

rm(list = ls())

library(ggplot2)

level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))

# Scatter changes, facet by location, color by year

ggplot(level_changes) +
  geom_point(aes(x = old_mean, y = new_mean, color = year_id)) +
  geom_abline(slope = 1, size = 0.5) +
  facet_wrap(~ihme_loc_id) +
  ggtitle("Locations with min 5% difference")
