# Test plotting of single band output
test_that("Plot basic output", {

  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Explicitly add grid specifying grid file name
  output$add_grid("../testdata/output/grid.bin.json")

  # Send to NULL file to avoid plots being written or shown
  pdf(file = NULL)

  # Plot a time series
  expect_message(
    plot(output),
    "spatial aggregation"
  )

  # If transformed it plots a raster plot
  output$transform(to = "lon_lat")
  expect_silent(
    plot(output)
  )

  dev.off()
})


# Test for errors raised by plotting function
test_that("Raise error plot basic output", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Dimension not existing
  expect_error(
    plot(output, aggregate = list(lat = mean)),
    "Undefined aggregation dimension"
  )

  # Not enough dimensions for plotting
  expect_error(
    plot(output, aggregate = list(cell = mean, time = mean)),
    "Only one dimensional data supplied"
  )

  # Time dimension has to be supplied with length > 1
  expect_error(
    plot(output, aggregate = list(time = mean)),
    "At least one temporal dimension of"
  )

  # Plot type "h" (and others) are not supported
  expect_error(
    plot(output, type = "h"),
    "Unsupported plot type"
  )

  # For pft_npp for a time series plot one has to reduce dimensions before plot
  expect_error(
    read_io(filename = "../testdata/output/pft_npp.bin.json") %>%
      plot(),
    "Too many dimensions for 2D time series plot"
  )

})
