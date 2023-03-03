# Test plotting of single band output
testthat::test_that("Plot basic output", {

  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Send to NULL file to avoid plots being written or shown
  pdf(file = NULL)

  # Plot a time series
  testthat::expect_message(
    plot(output),
    "spatial aggregation"
  )

  # If transformed it plots a raster plot
  output$transform(to = "lon_lat")
  testthat::expect_silent(
    plot(output)
  )

  dev.off()
})


# Test for errors raised by plotting function
testthat::test_that("Raise error plot basic output", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Dimension not existing
  testthat::expect_error(
    plot(output, aggregate = list(lat = mean)),
    "Undefined aggregation dimension"
  )

  # Not enough dimensions for plotting
  testthat::expect_error(
    plot(output, aggregate = list(cell = mean, time = mean)),
    "Only one dimensional data supplied"
  )

  # Time dimension has to be supplied with length > 1
  testthat::expect_error(
    plot(output, aggregate = list(time = mean)),
    "At least one temporal dimension of"
  )

  # Plot type "h" (and others) are not supported
  testthat::expect_error(
    plot(output, type = "h"),
    "Unsupported plot type"
  )

  # For pft_npp for a time series plot one has to reduce dimensions before plot
  testthat::expect_error(
    read_io(filename = "../testdata/output/pft_npp.bin.json") %>%
      plot(),
    "Too many dimensions for 2D time series plot"
  )

})
