
# utility function to test data integrity
test_integrity <- function(output) {

  # check for data
  dim_data <- dim(output$data)
  dimnames_data <- dimnames(output$data)
  #   check for cell dimension
  if ("cell" %in% names(dim_data)) {
    testthat::expect_equal(dim_data[["cell"]], output$meta$ncell)
    testthat::expect_equal(dimnames_data$cell,
                           as.character(
                             seq(output$meta$firstcell,
                                 length.out = output$meta$ncell)
                             )
                           )
  } else {
    testthat::expect_equal(dimnames_data$lat,
                           dimnames(output$grid)$lat)
    testthat::expect_equal(dimnames_data$lon,
                           dimnames(output$grid)$lon)
  }
  #   check for time dimension
  if ("time" %in% names(dim_data)) {
    testthat::expect_equal(dim_data[["time"]],
                           output$meta$nyear * output$meta$nstep)
    testthat::expect_equal(dimnames_data$time,
                           create_time_names(output$meta$nstep,
                                             seq(output$meta$firstyear,
                                                 length.out = output$meta$nyear))) # nolint
  } else {
    testthat::expect_equal(dim_data[["year"]], output$meta$nyear)
    testthat::expect_equal(dimnames_data$year,
                           as.character(
                             seq(output$meta$firstyear,
                                 length.out = output$meta$nyear)
                             )
                           )
    if ("month" %in% names(dim_data) && !output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(1:12))
    } else if ("month" %in% names(dim_data) && !output$meta$subset) {
      testthat::expect_equal(dimnames_data$month, as.character(6:9))
    }
  }
  #   check for band dimension
  testthat::expect_equal(dim_data[["band"]], output$meta$nbands)
  if (output$meta$nbands > 1) {
    testthat::expect_equal(dimnames_data$band, output$meta$band_names)
  }

  if (!is.null(output$grid)) {
    dimnames_grid <- dimnames(output$grid$data)
    if ("cell" %in% names(dimnames_grid)) {
      expect_equal(dimnames_grid$cell,
                   as.character(
                    seq(output$meta$firstcell, length.out = output$meta$ncell)
                   ))
    } else {
      expect_true(all(as.vector(na.omit(output$grid$data)) %in%
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
test_that("test transform_time method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform_time(to = "year_month_day")
  test_integrity(output)
})


# test transform_time method
test_that("test transform_space method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform_space(to = "lon_lat")
  test_integrity(output)
})


# test transform and subset method
test_that("test transform_space method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = c("year_month_day", "lon_lat"))
  output$subset(year = 2005:2008,
                month = 6:9,
                lat = c("55.25", "55.75", "56.25", "56.75"))
  output$transform(to = "cell")
  test_integrity(output)
})


# coordinates located within the last two cells
coordinates <- tibble::tibble(lat = c(55.9, 63.7),
                              lon = c(-87.3, -87.1))

# test subset method for coordinates (pair)
test_that("test transform_space method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$transform(to = c("lon_lat"))
  output$subset(coordinates = coordinates)
  output$transform(to = "cell")
  test_integrity(output)
})
