test_that("Calculate cell area", {
  # Dummy of latitude coordinates
  lats <- c(0, 24.5, 56.1, 89.4)
  # Expect vector of floating point values with same length as lats
  expect_type(calc_cellarea(lats), "double")
  expect_length(calc_cellarea(lats), length(lats))
})


test_that("Calculate cell area with LPJmLData object and grid attribute", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")
  # perform adding a grid object
  output$add_grid()
  # calculate cell area for each cell
  cell_area <- calc_cellarea(output, return_unit = "km2")

  # calculate cell area for lon_lat format
  output$transform(to = "lon_lat")
  cell_area2 <- calc_cellarea(output, return_unit = "km2")

  testthat::expect_equal(
    as.vector(cell_area),
    as.vector(cell_area2[match(names(cell_area), output$grid$data)])
  )
})

test_that("Calculate cell area with LPJmLData object of variable grid", {
  output <- read_io(filename = "../testdata/output/grid.bin.json")

  # calculate cell area for each cell
  cell_area <- calc_cellarea(output, return_unit = "km2")

  # calculate cell area for lon_lat format
  output$transform(to = "lon_lat")
  cell_area2 <- calc_cellarea(output, return_unit = "km2")

  testthat::expect_equal(
    as.vector(cell_area),
    as.vector(cell_area2[match(names(cell_area), output$grid$data)])
  )
})
