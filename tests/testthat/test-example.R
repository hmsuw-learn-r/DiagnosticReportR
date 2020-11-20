test_that("Example addition works", {

  expect_equal(example_add(1, 1), 2)
  expect_equal(example_add(1.5, 1:10), 2:11 + .5)

  expect_error(example_add("1", "2"))

})
