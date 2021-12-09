test_that("Calculate cell area", {
  # Dummy of latitude coordinates
  lats <- c(0, 24.5, 56.1, 89.4)
  # Expect vector of floating point values with same length as lats
  expect_type(calc_cellarea(lats), "double")
  expect_length(calc_cellarea(lats), length(lats))
})
