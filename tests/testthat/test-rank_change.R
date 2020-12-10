# Test rank_change function:

test_that("rank_change", {

  # level
  expect_message(
    rank_change(new_data,
                 old_data,
                 change_type = "level",
                 threshold = 10000,
                 comparison_var = "outcome",
                 id_vars = c("year", "group"),
                 group_vars = "group"),
    "There are no level changes at threshold: 10000 %"
    )

  expect_warning(
    rank_change(new_data_alt,
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
    rank_change(new_data,
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
    rank_change(new_data,
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
