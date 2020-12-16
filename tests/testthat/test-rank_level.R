# Test rank_level:

test_that("rank_level", {

  expect_message(
    rank_level(new_data,
               old_data,
               threshold = 10000,
               comparison_var = "outcome",
               id_vars = c("year", "group"),
               group_vars = "group"),
    "There are no level changes at threshold: 10000 %"
  )

  expect_warning(
    rank_level(new_data_alt,
               old_data,
               threshold = 5,
               comparison_var = "outcome",
               id_vars = c("year", "group"),
               group_vars = "group"),
    "new_data and old_data do not have the same number of rows. \n
            Comparisons will only be made by common id_vars"
  )

  expect_equal(
    rank_level(new_data,
               old_data,
               threshold = 5,
               comparison_var = "outcome",
               id_vars = c("year", "group"),
               group_vars = "group"),
    expected_level
  )

})
