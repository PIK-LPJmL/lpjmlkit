
# Utility function to test data integrity
#   tests designed for data to still have sequential order (c(1,2,3) NOT c(1,3))
#   latter would not work with following simplified tests
test_integrity <- function(output) {

  # Do call dim and dimnames only once
  dim_data <- dim(output$data)
  dimnames_data <- dimnames(output$data)

  # Check for two cases "cell" or "lon_lat"
  if ("cell" %in% names(dim_data)) {
    # Test for equal length of cell in data and meta data (ncell)
    testthat::expect_equal(dim_data[["cell"]], output$meta$ncell)
    # Test for equal dimnames of cell in data and those constructed by meta data
    testthat::expect_equal(
      dimnames_data$cell,
      format(
        seq(output$meta$firstcell, length.out = output$meta$ncell),
        trim = TRUE, scientific = FALSE
      )
    )
  } else {
    # Test for equal dimnames of lat, lon in data and those of underlying grid
    testthat::expect_equal(dimnames_data$lat,
                           dimnames(output$grid)$lat)
    testthat::expect_equal(dimnames_data$lon,
                           dimnames(output$grid)$lon)
  }

  # Check for two cases "time" or "year_month_day"
  if ("time" %in% names(dim_data)) {
    # Test for equal length of time steps in data and meta data (nyear * nstep)
    testthat::expect_equal(dim_data[["time"]],
                           output$meta$nyear * output$meta$nstep)
    # Test for equal dimnames of time steps in data and those constructed by
    #   meta data with create_time_names function (nstep, firstyear, nyear)
    testthat::expect_equal(
      dimnames_data$time,
      create_time_names(
        output$meta$nstep,
        seq(output$meta$firstyear, by = output$meta$timestep,
            length.out = output$meta$nyear)
      )
    )
  } else {
    # Test for equal length of years in data and meta data (nyear)
    testthat::expect_equal(dim_data[["year"]], output$meta$nyear)
    # Test for equal dimnames of years in data and those constructed by
    #   meta data (firstyear, nyear)
    testthat::expect_equal(
      dimnames_data$year,
      format(
        seq(output$meta$firstyear, by = output$meta$timestep,
            length.out = output$meta$nyear),
        trim = TRUE, scientific = FALSE
      )
    )

    # For month there is no meta data available (like nmonth, firstmonth)
    #   following tests only via hardcoded pre defined month to be tested
    if ("month" %in% names(dim_data) && !output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(1:12))
    } else if ("month" %in% names(dim_data) && output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(6:9))
    }
  }

  # Test for equal length of bands in data and meta data (nbands)
  testthat::expect_equal(dim_data[["band"]], output$meta$nbands)
  # Check if band dimension > 1 -> then has band_names
  if (output$meta$nbands > 1) {
    # Test for equal dimnames of band in data and those constructed by meta data
    #   (band_names)
    testthat::expect_equal(dimnames_data$band, output$meta$band_names)
  }

  # check for grid
  if (!is.null(output$grid)) {
    # Do call dimnames only once
    dimnames_grid <- dimnames(output$grid$data)

    # Check for two cases "cell" or "lon_lat"
    if ("cell" %in% names(dimnames_grid)) {
      # Test for equal dimnames of cell in grid data and those constructed by
      #   output meta data
      testthat::expect_equal(
        dimnames_grid$cell,
        format(
          seq(output$meta$firstcell, length.out = output$meta$ncell),
          trim = TRUE, scientific = FALSE
        )
      )
    } else {
      # Test to match data of grid (cell numbers) and cell numbers constructed
      #   by meta data of output
      testthat::expect_true(
        all(as.vector(stats::na.omit(output$grid$data)) %in%
            seq(output$meta$firstcell, length.out = output$meta$ncell))
      )
    }
  }
}


# test subset method
test_that("test subset method", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # Perform adding a grid object
  output$add_grid()
  # Read in grid directly
  output_sub <- subset(
    output,
    cell = 1:2,
    time = 1:5,
    band = c(
      "rainfed rice",
      "rainfed maize",
      "rainfed tropical cereals"
    )
  )
  test_integrity(output_sub)
})


# test transform_time method
test_that("test transform (time) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name) %>%
    transform(to = "year_month_day")
  test_integrity(output)

  output$transform(to = "time")
  test_integrity(output)

})


# test transform_time method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = "lon_lat")
  test_integrity(output)
})

# Test transform_time method
test_that("test non valid transform method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)

  # Invalid format provided
  expect_error(
    output$transform(to = "spacetime"),
    "Please choose from available space formats"
  )

  # No argument provided
  expect_error(
    output$transform(),
    "is missing, with no default"
  )
})


# test transform and subset method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = c("year_month_day", "lon_lat"))
  output$subset(year = as.character(2005:2008),
                month = 6:9,
                lat = c("55.25", "55.75", "56.25", "56.75"))
  output$transform(to = "cell")
  test_integrity(output)
})


# coordinates located within the last two cells
coordinates <- tibble::tibble(lat = c("55.9", "63.7"),
                              lon = c("-87.3", "-87.1"))

# test subset method for coordinates (pair)
test_that("test transform (space) method", {

  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output_trans <- transform(output, to = c("lon_lat"))
  output_trans$subset(coordinates = coordinates)
  output_trans$transform(to = "cell")
  test_integrity(output_trans)

  # For coordinate subsetting output has to be transformed first
  testthat::expect_error(
    output$subset(coordinates = coordinates),
    "convert into suitable format"
  )
})
