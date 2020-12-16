# Test plot_scatter and plot_trends functions

test_that("plot_scatter", {
  expect_error(plot_scatter(trend_changes_USA, 
                            x_var = old_mean, 
                            y_var = "new_mean", 
                            facet_x = "ihme_loc_id", 
                            pt_size = 2, 
                            facet_type = "wrap", 
                            fix_scale = TRUE, 
                            color_pt = "year_id"))
  expect_true(is.ggplot(plot_scatter(trend_changes_USA, 
                             x_var = "old_mean", 
                             y_var = "new_mean", 
                             facet_x = "ihme_loc_id", 
                             pt_size = 2, 
                             facet_type = "wrap", 
                             fix_scale = TRUE, 
                             color_pt = "year_id")))
}
)

test_that("plot_trends", {
  expect_error(plot_trends(trend_changes_USA, 
                           time_var = "year_id", 
                           y1_var = "old_mean", 
                           y2_var = "new_mean", 
                           facet_x = "ihme_loc_id",
                           facet_type = wrap,
                           line_size = 2))
  expect_true(is.ggplot(plot_trends(trend_changes_USA, 
                                    time_var = "year_id", 
                                    y1_var = "old_mean", 
                                    y2_var = "new_mean", 
                                    facet_x = "ihme_loc_id",
                                    facet_type = "wrap",
                                    line_size = 2)))
}
)