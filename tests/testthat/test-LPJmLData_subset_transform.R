
# utility function to test data integrity
#   tests designed for data to still have sequential order (c(1,2,3) NOT c(1,3))
#   latter would not work with following simplified tests
test_integrity <- function(output) {

  # do call dim and dimnames only once
  dim_data <- dim(output$data)
  dimnames_data <- dimnames(output$data)

  # check for two cases "cell" or "lon_lat"
  if ("cell" %in% names(dim_data)) {
    # test for equal length of cell in data and meta data (ncell)
    testthat::expect_equal(dim_data[["cell"]], output$meta$ncell)
    # test for equal dimnames of cell in data and those constructed by meta data
    testthat::expect_equal(
      dimnames_data$cell,
      format(
        seq(output$meta$firstcell, length.out = output$meta$ncell),
        trim = TRUE, scientific = FALSE, justify = "none"
      )
    )
  } else {
    # test for equal dimnames of lat, lon in data and those of underlying grid
    testthat::expect_equal(dimnames_data$lat,
                           dimnames(output$grid)$lat)
    testthat::expect_equal(dimnames_data$lon,
                           dimnames(output$grid)$lon)
  }

  # check for two cases "time" or "year_month_day"
  if ("time" %in% names(dim_data)) {
    # test for equal length of time steps in data and meta data (nyear * nstep)
    testthat::expect_equal(dim_data[["time"]],
                           output$meta$nyear * output$meta$nstep)
    # test for equal dimnames of time steps in data and those constructed by
    #   meta data with create_time_names function (nstep, firstyear, nyear)
    testthat::expect_equal(dimnames_data$time,
                           create_time_names(output$meta$nstep,
                                             seq(output$meta$firstyear,
                                                 length.out = output$meta$nyear))) # nolint
  } else {
    # test for equal length of years in data and meta data (nyear)
    testthat::expect_equal(dim_data[["year"]], output$meta$nyear)
    # test for equal dimnames of years in data and those constructed by
    #   meta data (firstyear, nyear)
    testthat::expect_equal(dimnames_data$year,
                           as.character(
                             seq(output$meta$firstyear,
                                 length.out = output$meta$nyear)
                             )
                           )

    # for month there is no meta data available (like nmonth, firstmonth)
    #   following tests only via hardcoded pre defined month to be tested
    if ("month" %in% names(dim_data) && !output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(1:12))
    } else if ("month" %in% names(dim_data) && !output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(6:9))
    }
  }

  # test for equal length of bands in data and meta data (nbands)
  testthat::expect_equal(dim_data[["band"]], output$meta$nbands)
  # check if band dimension > 1 -> then has band_names
  if (output$meta$nbands > 1) {
    # test for equal dimnames of band in data and those constructed by meta data
    #   (band_names)
    testthat::expect_equal(dimnames_data$band, output$meta$band_names)
  }

  # check for grid
  if (!is.null(output$grid)) {
    # do call dimnames only once
    dimnames_grid <- dimnames(output$grid$data)

    # check for two cases "cell" or "lon_lat"
    if ("cell" %in% names(dimnames_grid)) {
      # test for equal dimnames of cell in grid data and those constructed by
      #   output meta data
      expect_equal(dimnames_grid$cell,
                   as.character(
                    seq(output$meta$firstcell, length.out = output$meta$ncell)
                   ))
    } else {
      # test to match data of grid (cell numbers) and cell numbers constructed
      #   by meta data of output
      expect_true(all(as.vector(stats::na.omit(output$grid$data)) %in%
                      seq(output$meta$firstcell, length.out = output$meta$ncell)) # nolint
                  )
    }
  }
}


# test subset method
test_that("test subset method", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # perform adding a grid object
  output$add_grid()
  # read in grid directly
  output$subset(cell = 1:2,
                time = 1:5,
                band = c("rainfed rice",
                         "rainfed maize",
                         "rainfed tropical cereals"))
  test_integrity(output)
})


# test transform_time method
test_that("test transform (time) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = "year_month_day")
  test_integrity(output)
})


# test transform_time method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = "lon_lat")
  test_integrity(output)
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
  output$transform(to = c("lon_lat"))
  output$subset(coordinates = coordinates)
  output$transform(to = "cell")
  test_integrity(output)
})
