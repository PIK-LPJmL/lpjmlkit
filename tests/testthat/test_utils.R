test_that("Test argument deprecation", {

  # Set values
  new_arg <- NULL
  deprec_arg <- "test"

  # Test if deprec_arg replaces new_arg
  new_arg <- suppressMessages(
    deprecate_arg(
      new_arg = new_arg,
      deprec_arg = deprec_arg
    )
  )
  testthat::expect_equal(new_arg, deprec_arg)

  new_arg <- NULL
  # Test for deprecation message
  testthat::expect_message(
    deprecate_arg(
      new_arg = new_arg,
      deprec_arg = deprec_arg
    ),
    "is deprecated as of version"
  )

  # Test for prior usage of new_arg
  new_arg <- "better_test"
  new_arg <- suppressMessages(
    suppressWarnings(
      deprecate_arg(
        new_arg = new_arg,
        deprec_arg = deprec_arg
      )
    )
  )
  testthat::expect_equal(new_arg, new_arg)

  # Test corresponding message
  new_arg <- "better_test"
  suppressMessages(
    testthat::expect_warning(
      deprecate_arg(
        new_arg = new_arg,
        deprec_arg = deprec_arg
      ),
      "will be ignored in favour of argument"
    )
  )
})
