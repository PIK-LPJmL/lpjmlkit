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

  # Non-supported type for res_lon
  testthat::expect_error(
    calc_cellarea(lats, res_lon = NA),
    "Invalid longitude grid"
  )

  # Vector > 1 not supported for res_lat
  testthat::expect_warning(
    calc_cellarea(lats, res_lat = c(0.5, 0.5)),
    "res_lat has length"
  )

  # Non-supported type for res_lat
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

  # Add a grid object
  output$add_grid("../testdata/output/grid.bin.json")

  # Calculate cell area for each cell
  cell_area <- calc_cellarea(output, return_unit = "km2")

  # Calculate cell area for lon_lat format
  output$transform(to = "lon_lat")
  cell_area2 <- calc_cellarea(output, return_unit = "km2")

  testthat::expect_equal(
    as.vector(cell_area),
    as.vector(cell_area2[match(names(cell_area), output$grid$data)])
  )

  # Test that function arguments res_lon and res_lat are ignored for LPJmLData
  testthat::expect_warning(
    calc_cellarea(output, res_lon = 0.25),
    "Using .+ instead of supplied res_lon"
  )
  testthat::expect_warning(
    calc_cellarea(output, res_lat = 0.25),
    "Using .+ instead of supplied res_lat"
  )

  # Test alternative dim_order
  output <- read_io(
    filename = "../testdata/output/npp.bin.json",
    dim_order = c("time", "band", "cell")
  )
  output$add_grid("../testdata/output/grid.bin.json")
  cell_area3 <- calc_cellarea(output, return_unit = "km2")
  testthat::expect_identical(cell_area, cell_area3)

  # Test for Object of class LPJmLGridData
  output <- read_io("../testdata/output/grid.bin.json") %>% LPJmLGridData$new()
  cell_area4 <- calc_cellarea(output, return_unit = "km2")
  testthat::expect_identical(cell_area, cell_area4)

  # Test for grid file read as LPJmLData
  output <- read_io("../testdata/output/grid.bin.json")
  cell_area5 <- calc_cellarea(output, return_unit = "km2")
  testthat::expect_identical(cell_area, cell_area5)
})

test_that("Calculate cell area with LPJmLData object of variable grid", {
  output <- read_io(filename = "../testdata/output/grid.bin.json")
  # Calculate cell area for each cell
  cell_area <- calc_cellarea(output, return_unit = "km2")

  # Add a grid object for spatial transformation (supply different dim_order)
  output$add_grid(
    "../testdata/output/grid.bin.json",
    dim_order = c("band", "time", "cell")
  )

  # Calculate cell area for lon_lat format
  output$transform(to = "lon_lat")
  cell_area2 <- calc_cellarea(output, return_unit = "km2")

  testthat::expect_equal(
    as.vector(cell_area),
    as.vector(cell_area2[match(names(cell_area), output$grid$data)])
  )
})
