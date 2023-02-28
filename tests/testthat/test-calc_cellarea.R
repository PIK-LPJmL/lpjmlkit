test_that("Calculate cell area", {
  # Dummy of latitude coordinates
  lats <- c(0, 24.5, 56.1, 89.4)
  # Expect vector of floating point values with same length as lats
  testthat::expect_type(calc_cellarea(lats), "double")
  testthat::expect_length(calc_cellarea(lats), length(lats))

  # Vector > 1 not supported for res_lon
  testthat::expect_warning(
    calc_cellarea(lats, res_lon = c(0.5, 0.5)),
    "res_lon has length"
  )

  # Non supported type for res_lon
  testthat::expect_error(
    calc_cellarea(lats, res_lon = NA),
    "Invalid longitude grid"
  )

  # Vector > 1 not supported for res_lat
  testthat::expect_warning(
    calc_cellarea(lats, res_lat = c(0.5, 0.5)),
    "res_lat has length"
  )

  # Non supported type for res_lat
  testthat::expect_error(
    calc_cellarea(lats, res_lat = NA),
    "Invalid latitude grid"
  )

  # Latitudes > 90 not valid
  lats <- c(0, 24.5, 56.1, 99.4)
  testthat::expect_error(
    calc_cellarea(lats),
    "Invalid latitude values in 'x'"
  )
})


test_that("Calculate cell area with LPJmLData object and grid attribute", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Grid attribute missing
  testthat::expect_error(
    calc_cellarea(output),
    "Grid attribute is missing"
  )

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
