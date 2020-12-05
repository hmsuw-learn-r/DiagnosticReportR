# Testing plots to review/compare time trends from level changes

rm(list=ls())

library(ggplot2)

level_changes <- as.data.table(read.csv("/ihme/scratch/users/erinam/520_proj/level_changes.csv"))

# Plot level changes, facet by location
ggplot(level_changes, aes(x = year_id)) +
  geom_line(aes(y = old_mean, color = "previous"), size = 2) +
  geom_line(aes(y = new_mean, color = "new"), size = 2) +
  labs(color = "Model version") +
  xlab("year") +
  ylab("birth sex ratio") +
  scale_color_manual(values = c("previous" = "dodgerblue", "new" = "deeppink4")) +
  guides(color = guide_legend(reverse = TRUE)) +
  facet_wrap(~ihme_loc_id) +
  ggtitle("Locations with 5% difference")

# Test highlight arbitrary section of plot
ggplot(level_changes[ihme_loc_id == "BWA"], aes(x = year_id)) +
  geom_line(aes(y = old_mean, color = "previous"), size = 2) +
  geom_line(aes(y = new_mean, color = "new"), size = 2) +
  labs(color = "Model version") +
  xlab("year") +
  ylab("birth sex ratio") +
  scale_color_manual(values = c("previous" = "dodgerblue", "new" = "deeppink4")) +
  guides(color = guide_legend(reverse = TRUE)) +
  annotate("rect", xmin=1983, xmax=2000, ymin=1, ymax=Inf, alpha=0.2, fill="red")
