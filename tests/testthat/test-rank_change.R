# Test rank_changes function:

# Inputs
old_data <- data.table::data.table(year = rep(1:3, 3),
                                   group = rep(c("a", "b", "c"), each = 3),
                                   outcome = c(1, 2, 3, 3, 2, 1, 1, 2, 3))

old_data_alt <- data.table::data.table(year = rep(1:3, 3),
                                       group = rep(c("a", "b", "c"), each = 3),
                                       outcome = c(2, 4, 6, 4, 8, 12, 1, 2, 3))

new_data <- data.table::data.table(year = rep(1:3, 3),
                                   group = rep(c("a", "b", "c"), each = 3),
                                   outcome = c(2, 4, 6, 12, 8, 4, 0, 0, 0))

new_data_alt <- data.table::data.table(year = rep(1:3, 2),
                                       group = rep(c("a", "b"), each = 3),
                                       outcome = c(2, 4, 6, 0, 0, 0))

expected_level <- data.table::data.table(year = rep(1:3, 3),
                                         group = rep(c("b", "c", "a"), each = 3),
                                         new_outcome = c(12, 8, 4, 0, 0, 0, 2, 4, 6),
                                         old_outcome = c(3, 2, 1, 1, 2, 3, 1, 2, 3),
                                         pert_diff = c(rep(300, 3), rep(-100, 3),
                                                       rep(100, 3)),
                                         max_abs_pert_diff = c(rep(300, 3),
                                                           rep(100, 6)),
                                         mean_abs_pert_diff = c(rep(300, 3),
                                                               rep(100, 6)),
                                         rank = rep(c(1, 2, 2), each = 3))

expected_trend <- data.table::data.table(group = rep(c("b", "c", "a"), each = 3),
                                         year = rep(1:3, 3),
                                         new_outcome = c(12, 8, 4, 0, 0, 0, 2, 4, 6),
                                         old_outcome = c(4, 8, 12, 1, 2, 3, 2, 4, 6),
                                         bin = rep(1, 9),
                                         old_slope = c(rep(4, 3), rep(1, 3),
                                                       rep(2, 3)),
                                         new_slope = c(rep(-4, 3), rep(0, 3),
                                                       rep(2, 3)),
                                         sign_change = c(rep(1, 3), rep(0, 6)),
                                         pert_diff = c(rep(-200, 3),
                                                       rep(-100, 3),
                                                       rep(0, 3)),
                                         mean_abs_pert_diff = c(rep(200, 3),
                                                                rep(100, 3),
                                                                rep(0, 3)),
                                         numb_sign_change = c(rep(1, 3),
                                                              rep(0, 6)),
                                         rank = rep(c(1, 2, 3), each = 3))

# Expected outputs

test_that("rank_changes", {

  # level
  expect_message(
    rank_changes(new_data,
                 old_data,
                 change_type = "level",
                 threshold = 10000,
                 comparison_var = "outcome",
                 id_vars = c("year", "group"),
                 group_vars = "group"),
    "There are no level changes at threshold: 10000 %"
    )

  expect_warning(
    rank_changes(new_data_alt,
                 old_data,
                 change_type = "level",
                 threshold = 5,
                 comparison_var = "outcome",
                 id_vars = c("year", "group"),
                 group_vars = "group"),
    "new_data and old_data do not have the same number of rows. \n
            Comparisons will only be made by common id_vars"
  )

  expect_equal(
    rank_changes(new_data,
                 old_data,
                 change_type = "level",
                 threshold = 5,
                 comparison_var = "outcome",
                 id_vars = c("year", "group"),
                 group_vars = "group"),
    expected_level
    )

  # trend
  expect_equal(
    rank_changes(new_data,
                 old_data_alt,
                 change_type = "trend",
                 threshold = 1L,
                 comparison_var = "outcome",
                 id_vars = c("year", "group"),
                 group_vars = "group",
                 trend_var = "year"),
    expected_trend
  )

})
