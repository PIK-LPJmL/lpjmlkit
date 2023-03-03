# test subset_array function
test_that("asub", {
  my_array <- array(1,
                    dim = c(cell = 67, month = 12, band = 3),
                    dimnames = list(cell = 0:66,
                                  month = 1:12,
                                  band = c("band1", "band2", "band3")))
  my_subset <- asub(my_array,
                    band = c("band1", "band3"))
  testthat::expect_equal(
    dimnames(my_subset),
    list(
      cell = as.character(0:66),
      month = as.character(1:12),
      band = c("band1", "band3")
    )
  )

  # Test warning message where one subset remains valid
  testthat::expect_warning(
    subset_array(my_array, list(band = c("band1", NA)), silent = FALSE),
    "Removing NA values from"
  )

  # Test warning message where a subset is empty
  testthat::expect_warning(
    subset_array(my_array, list(month = c()), silent = FALSE),
    "empty."
  )

  # Test warning message where no subset remains valid
  testthat::expect_warning(
    subset_array(my_array, list(month = NA), silent = FALSE),
    "Removing NA values from"
  ) %>% suppressWarnings()

  # Test error message where no subset remains valid
  testthat::expect_error(
    asub(my_array, hour = 2),
    "Please choose from available dimension names"
  )

  # Test error message where no subset remains valid
  testthat::expect_error(
    asub(my_array, month = 42),
    "not valid."
  )

  # Test warning messages where no subset remains valid
  testthat::expect_error(
    asub(my_array, band = "band4"),
    "not valid."
  )

  my_2nd_subset <- asub(my_subset,
                         month = c("5"),
                         drop = TRUE)
  expect_equal(dimnames(my_2nd_subset),
               list(cell = as.character(0:66),
                    band = c("band1", "band3")))

  # replace subset
  asub(my_2nd_subset, band = c("band1")) <- 0
  testthat::expect_true(all(my_2nd_subset[, 1] == 0))

  testthat::expect_true(length(which(my_2nd_subset == 0)) == 67)

})
