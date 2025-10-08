# Test as_array method (basic way)
test_that("basic array export", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)
  output_array <- output$as_array()

  output$transform(to = "lon_lat")

  expect_true(
    all(
      output_array %in% as.numeric(unlist(dimnames(output$data)))
    )
  )
})


# test as_tibble method (basic way)
test_that("basic tibble export", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)
  output_tibble <- tibble::as_tibble(output)

  # test for equality with test_tibble
  expect_equal(output_tibble$value, as.vector(output$data))
})


# test as_raster method with lon lat dimensions
test_that("raster export lon_lat", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  output_raster <- as_raster(output)

  output2 <- transform(output, to = "lon_lat")
  replace_array <- output2$data
  # create tmp_raster with expected dimensions and coordinate ref system
  test_raster <- raster::raster(resolution = 0.5,
                                crs = raster_crs_fallback("EPSG:4326"),
                                xmn = -87.5,
                                xmx = -87,
                                ymn = 55,
                                ymx = 64) %>%
    raster::brick(nl = 2)

  # replace values in tmp_raster with expected values of longitude
  replace_array[which(!is.na(replace_array))] <- rev(
    subset(output, band = "lon")$data
  )
  test_raster <- raster::setValues(
    test_raster,
    values = replace_array,
    layer = 1
  )

  # replace values in tmp_raster with expected values of latitude
  replace_array[which(!is.na(replace_array))] <- rev(
    subset(output, band = "lat")$data
  )
  test_raster <- raster::setValues(
    test_raster,
    values = replace_array,
    layer = 2
  )

  raster::crs(test_raster) <- raster_crs_fallback("EPSG:4326")
  # add variable as layer name
  names(test_raster) <- names(dim(output2))

  # test for equality with test_raster
  expect_equal(output_raster, test_raster)


  test_raster2 <- output2$as_raster()

  # test raster matrix values with those of transformed grid
  expect_equal(as.vector(test_raster2[]), as.vector(output2$data))
})


# test as_terra method with lon lat dimensions
test_that("terra export lon_lat", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  output_rast <- as_terra(output)

  output2 <- transform(output, to = "lon_lat")
  replace_array <- array(NA, dim = c(length(output2$data), 2))


  test_rast <- terra::rast(
    crs = "EPSG:4326",
    extent = terra::ext(-87.5, -87, 55, 64),
    resolution = 0.5,
    nlyrs = 2
  )

  # replace values in tmp_rast with expected values of longitude and latitude
  replace_array[which(!is.na(output2$data)), 1] <- rev(subset(output, band = "lon")$data)
  replace_array[which(!is.na(output2$data)), 2] <- rev(subset(output, band = "lat")$data)
  test_rast <- terra::setValues(test_rast, replace_array)

  # add variable as layer name
  names(test_rast) <- names(dim(output2))
  terra::units(test_rast) <- output$meta$unit

  # test for equality with test_rast
  expect_equal(output_rast[], test_rast[])

  test_raster2 <- output2$as_raster()

  # test raster matrix values with those of transformed grid
  expect_equal(as.vector(test_raster2[]), as.vector(output2$data))

})
