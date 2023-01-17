# test subset_array function
test_that("asub", {
  my_array <- array(1,
                    dim = c(cell = 67, month = 12, band = 3),
                    dimnames = list(cell = 0:66,
                                  month = 1:12,
                                  band = c("band1", "band2", "band3")))
  my_subset <- asub(my_array,
                    band = c("band1", "band3"))
  expect_equal(dimnames(my_subset),
               list(cell = as.character(0:66),
                    month = as.character(1:12),
                    band = c("band1", "band3")))

  my_2nd_subset <- asub(my_subset,
                         month = c("5"),
                         drop = TRUE)
  expect_equal(dimnames(my_2nd_subset),
               list(cell = as.character(0:66),
                    band = c("band1", "band3")))

  # replace subset
  asub(my_2nd_subset, band = c("band1")) <- 0
  expect_true(all(my_2nd_subset[, 1] == 0))

  expect_true(length(which(my_2nd_subset == 0)) == 67)

})
