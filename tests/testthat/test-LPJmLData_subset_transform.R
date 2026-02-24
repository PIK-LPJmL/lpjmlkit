
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
        all(c(stats::na.omit(c(drop_omit(output$grid$data, c("lon", "lat"))))) %in% # nolint
              seq(output$meta$firstcell, length.out = output$meta$ncell))
      )
    }
  }
}


# test subset method
test_that("test subset method", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # Perform auto-loading a grid object, which produces a message
  expect_message(
    output$add_grid(),
    "grid.bin.json"
  )

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

  output2 <- read_io(
    filename = file_name,
    dim_order = c("time", "band", "cell")
  ) %>% transform(to = "year_month_day")
  test_integrity(output2)
  expect_identical(
    output2$data,
    aperm(output$data, names(dim(output2)))
  )

  output$transform(to = "time")
  test_integrity(output)

  output2$transform(to = "time")
  test_integrity(output2)
  expect_identical(
    output2$data,
    aperm(output$data, names(dim(output2)))
  )

  output$transform("year_month_day")
  # remove a month, check that remaining data is not affected
  output$subset(month = -11)
  output$transform("time")
  expect_identical(
    subset(output2, time = dimnames(output)$time)$data,
    aperm(output$data, names(dim(output2)))
  )
})


# test transform_space method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  # transform auto-loads grid, which produces a message
  expect_message(
    output$transform(to = "lon_lat"),
    "grid.bin.json"
  )
  test_integrity(output)

  output2 <- read_io(
    filename = file_name,
    dim_order = c("time", "band", "cell")
  )
  expect_message(
    output2$transform(to = "lon_lat"),
    "grid.bin.json"
  )
  test_integrity(output2)
  expect_identical(
    output2$data,
    aperm(output$data, names(dim(output2)))
  )
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
  # Explicitly load grid
  output$add_grid("../testdata/output/grid.bin.json")
  output$transform(to = c("year_month_day", "lon_lat"))
  output$subset(year = 5:8,
                month = 6:9,
                lat = c("55.25", "55.75", "56.25", "56.75"))
  output$transform(to = "cell")
  test_integrity(output)

  output2 <- read_io(
    filename = file_name,
    dim_order = c("time", "band", "cell")
  )
  output2$add_grid("../testdata/output/grid.bin.json")
  output2$transform(to = c("year_month_day", "lon_lat"))
  output2$subset(year = as.character(2005:2008),
                 month = as.character(6:9),
                 lat = c("55.25", "55.75", "56.25", "56.75"))
  output2$transform(to = "cell")
  test_integrity(output2)
  expect_identical(
    output2$data,
    aperm(output$data, names(dim(output2)))
  )
})


# coordinates located within the last two cells
coordinates <- tibble::tibble(
  lat = c("55.9", "63.7"),
  lon = c("-87.3", "-87.1")
)
coordinates_numeric <- tibble::tibble(
  lon = as.numeric(coordinates$lon),
  lat = as.numeric(coordinates$lat)
)

# test subset method for coordinates (pair)
test_that("test transform (space) method", {

  file_name <- "../testdata/output/transp.bin.json"
  output <- read_io(filename = file_name)
  output$add_grid("../testdata/output/grid.bin.json")
  output_trans <- transform(output, to = c("lon_lat"))
  output_trans$subset(coordinates = coordinates)
  output_back <- transform(output_trans, to = "cell")
  test_integrity(output_trans)
  test_integrity(output_back)
  if (!anyNA(output$data)) {
    expect_false(anyNA(output_trans$data))
  }

  output2 <- read_io(
    filename = file_name,
    dim_order = c("time", "band", "cell")
  )
  output2$add_grid("../testdata/output/grid.bin.json")
  output_trans2 <- transform(output2, to = "lon_lat")
  output_trans2$subset(coordinates = coordinates)
  output_back2 <- transform(output_trans2, to = "cell")
  test_integrity(output_trans2)
  test_integrity(output_back2)
  if (!anyNA(output2$data)) {
    expect_false(anyNA(output_trans2$data))
  }
  expect_identical(
    output_trans2$data,
    aperm(output_trans$data, names(dim(output_trans2)))
  )
  expect_identical(
    output_back2$data,
    aperm(output_back$data, names(dim(output_back2)))
  )

  # For coordinate subsetting output has to be transformed first
  expect_error(
    output$subset(coordinates = coordinates),
    "convert into suitable format"
  )

  # Coordinate values must be strings, not numerical values
  output_trans <- transform(output, to = c("lon_lat"))
  expect_error(
    output_trans$subset(coordinates = coordinates_numeric),
    "Values for coordinate pairs must be supplied as strings"
  )
})

# test transforming daily data into different time formats
test_that("test transform (time) method", {

  file_name <- "../testdata/input/tas_gswp3-w5e5_obsclim_2015-2019.clm"
  tas <- read_io(filename = file_name)

  # transformation to "year_month_day" format
  tas_time_transformed <- transform(tas, "year_month_day")
  expect_true(all(is.na(tas_time_transformed$data[, 31, 2, , ])))

  # back transformation to time format
  tas_time_transformed$transform("time")
  # check against original data
  expect_identical(tas$data, tas_time_transformed$data)

  # add grid for spatial transformation
  tas$add_grid("../testdata/output/grid.bin.json")
  # test NA handling of days in combination with spatial transformation
  #   includes additional NAs in lat, lon matrix
  tas_transformed <- transform(tas, "lat_lon")

  # transformation to "year_month_day" format
  tas_transformed$transform("year_month_day")
  expect_true(all(is.na(tas_transformed$data[, , 31, 2, , ])))

  # back transformation to time format
  tas_transformed$transform("time")
  # back transformation to cell format
  tas_transformed$transform("cell")

  # check against original data
  expect_identical(tas$data, tas_time_transformed$data)

  # test subsetting daily data with gaps in time and random shuffling
  subset_tas <- subset(tas, time = sample(dim(tas)["time"])[-c(10, 200)])
  missing_timestep <- setdiff(dimnames(tas)$time, dimnames(subset_tas)$time)

  # transformation to "year_month_day" format, avoid NA warning
  expect_warning(
    tas_subset_transformed <- transform(subset_tas, "year_month_day"),
    "gaps"
  )
  # back transformation to time format
  tas_subset_transformed$transform("time")

  # Timesteps removed randomly before transformation have been reintroduced
  # containing NAs.
  expect_true(
    all(
      is.na(subset(tas_subset_transformed, time = missing_timestep)$data)
    )
  )
  # Non-removed timesteps should be restored to original sequence after
  # transformation and be identical with original data.
  remaining_time_steps <- setdiff(
    dimnames(tas_subset_transformed)$time,
    missing_timestep
  )
  expect_identical(
    subset(tas, time = remaining_time_steps)$data,
    subset(tas_subset_transformed, time = remaining_time_steps)$data
  )
})
