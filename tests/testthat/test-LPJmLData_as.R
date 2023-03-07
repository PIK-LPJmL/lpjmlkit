# test as_array method (basic way)
test_that("basic array export", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  output_array <- output$as_array()

  # read test_array
  test_array <- readRDS("../testdata/test_array.rds")

  # test for equality with test_array
  testthat::expect_equal(output_array, test_array)
})


# test as_array method with subset and aggregate functionality
test_that("array export with subset and aggregate", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  output_array <- as_array(
    output,
    subset = list(band = "boreal needleleaved evergreen tree"),
    aggregate = list(time = mean)
  )

  # read and subset, aggregate test_array
  test_array <- readRDS("../testdata/test_array.rds") %>%
    subset_array(
      list(band = "boreal needleleaved evergreen tree"),
      drop = FALSE
    ) %>%
    apply(c("cell", "band"), mean)

  # test for equality with test_array on which operations have been performed on
  #   manually with explicit subset_array and apply calls
  testthat::expect_equal(output_array, test_array)
})


# test as_tibble method (basic way)
test_that("basic tibble export", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  output_tibble <- as_tibble(output)

  # read test_array and convert to tibble manually
  test_data <- readRDS("../testdata/test_array.rds")
  test_tibble <- test_data %>%
    reshape2::melt() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(across(names(dimnames(test_data)), as.factor))

  # test for equality with test_tibble
  testthat::expect_equal(output_tibble, test_tibble)
})


# test as_raster method with lon lat dimensions
test_that("raster export lon_lat", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # transform auto-loads grid, which produces a message
  testthat::expect_message(
    output$transform(to = "lon_lat"),
    "grid.bin.json"
  )

  output_raster <- as_raster(
    output,
    subset = list(band = "boreal needleleaved evergreen tree"),
    aggregate = list(time = mean)
  )

  # create tmp_raster with expected dimensions and coordinate ref system
  tmp_raster <- raster::raster(res = 0.5,
                               crs = "EPSG:4326",
                               xmn = -87.5,
                               xmx = -87,
                               ymn = 55,
                               ymx = 64)
  # read test_array with lon lat instead of cell dimension
  # subset only one band and aggregate time dimension
  test_data <- readRDS("../testdata/test_array_lonlat.rds") %>%
    subset_array(
      list(band = "boreal needleleaved evergreen tree"),
      drop = FALSE
    ) %>%
    apply(c("lon", "lat"), mean)
  # lat dimension has to be reverted to convert to raster
  test_raster <- raster::raster(
    test_data[, seq_len(dim(test_data)[["lat"]]), drop = FALSE],
    template = tmp_raster
  )
  # add variable as layer name
  names(test_raster) <- output$meta$variable

  # test for equality with test_raster
  testthat::expect_equal(output_raster, test_raster)
})


# test as_raster method with 3rd dimension (brick)
test_that("raster export 3rd dim", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # as_raster auto-loads grid, which produces a message
  testthat::expect_message(
    output_raster <- output$as_raster(
      subset = list(band = "boreal needleleaved evergreen tree")
    ),
    "grid.bin.json"
  )

  # create tmp_raster with expected dimensions and coordinate ref system
  tmp_raster <- raster::raster(res = 0.5,
                               crs = "EPSG:4326",
                               xmn = -87.5,
                               xmx = -87,
                               ymn = 55,
                               ymx = 64)
  # create raster brick based on length of time dimension
  test_raster <- raster::brick(tmp_raster,
                               nl = output$meta$nyear * output$meta$nstep)
  # read test_array with cell dimension and subset only one band
  test_data <- readRDS("../testdata/test_array.rds") %>%
    subset_array(
      list(band = "boreal needleleaved evergreen tree"),
      drop = FALSE
    )

  # add values of raster cells by corresponding coordinates (lon, lat)
  test_raster[
    raster::cellFromXY(
      test_raster,
      cbind(subset_array(output$grid$data, list(band = "lon")),
            subset_array(output$grid$data, list(band = "lat")))
    )
  ] <- test_data

  # add time dimension naming as layer names
  names(test_raster) <- dimnames(output$data)$time

  # test for equality with test_raster
  testthat::expect_equal(output_raster, test_raster)
})



# test as_terra method with lon lat dimensions
test_that("terra export lon_lat", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # Explicitly add grid specifying grid file name
  output$add_grid("../testdata/output/grid.bin.json")
  output$transform(to = "lon_lat")

  output_rast <- as_terra(
    output,
    subset = list(band = "boreal needleleaved evergreen tree"),
    aggregate = list(time = mean)
  )

  # read test_array with lon lat instead of cell dimension
  # subset only one band and aggregate time dimension
  test_data <- readRDS("../testdata/test_array_lonlat.rds") %>%
    subset_array(
      list(band = "boreal needleleaved evergreen tree"),
      drop = FALSE
    ) %>%
    apply(c("lat", "lon"), mean)
  # lat dimension has to be reverted to convert to rast
  test_rast <- terra::rast(
    test_data[rev(seq_len(dim(test_data)[["lat"]])), , drop = FALSE],
    crs = "EPSG:4326",
    extent = terra::ext(-87.5, -87, 55, 64)
  )
  # add variable as layer name
  names(test_rast) <- output$meta$variable
  terra::units(test_rast) <- output$meta$unit

  # test for equality with test_rast
  # use base::all.equal to avoid pointer mismatches
  testthat::expect_true(all.equal(output_rast, test_rast))
})


# test as_terra method with 3rd dimension (brick)
test_that("terra export 3rd dim", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  # Explicitly add grid specifying grid file name
  output$add_grid("../testdata/output/grid.bin.json")
  output_rast <- output$as_terra(
    subset = list(band = "boreal needleleaved evergreen tree")
  )

  # create tmp_rast with expected dimensions and coordinate ref system
  test_rast <- terra::rast(
    res = 0.5,
    crs = "EPSG:4326",
    xmin = -87.5,
    xmax = -87,
    ymin = 55,
    ymax = 64,
    nlyrs = output$meta$nyear * output$meta$nstep
  )
  # read test_array with cell dimension and subset only one band
  test_data <- readRDS("../testdata/test_array.rds") %>%
    subset_array(
      list(band = "boreal needleleaved evergreen tree"),
      drop = FALSE
    )

  # add values of rast cells by corresponding coordinates (lon, lat)
  test_rast[
    terra::cellFromXY(
      test_rast,
      cbind(subset_array(output$grid$data, list(band = "lon")),
            subset_array(output$grid$data, list(band = "lat")))
    )
  ] <- test_data

  # add time dimension naming as layer names
  names(test_rast) <- dimnames(output$data)$time
  terra::units(test_rast) <- output$meta$unit
  terra::time(test_rast) <- as.Date(
    dimnames(output$data)$time
  )

  # test for equality with test_rast
  # use base::all.equal to avoid pointer mismatches
  testthat::expect_true(all.equal(output_rast, test_rast))
})
