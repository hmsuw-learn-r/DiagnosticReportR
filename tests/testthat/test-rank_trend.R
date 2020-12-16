# Test rank_level:

test_that("rank_trend", {

  expect_equal(
    rank_trend(new_data,
               old_data_alt,
               numb_bins = 1L,
               comparison_var = "outcome",
               id_vars = c("year", "group"),
               group_vars = "group",
               trend_var = "year"),
    expected_trend
  )

})
