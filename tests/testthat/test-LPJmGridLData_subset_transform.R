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
  }

  if (output$meta$._space_format_ == "cell") {
    # Test for equal length of bands in data and meta data (nbands)
    testthat::expect_equal(dim_data[["band"]], output$meta$nbands)
  }
}


# test subset method
test_that("test subset method", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  output_sub <- subset(
    output,
    cell = 1:2
  )
  test_integrity(output_sub)
})


# test transform_time method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  test_integrity(output)

  output2 <- read_grid(
    filename = file_name,
    dim_order = c("band", "cell", "time")
  )

  output2$transform(to = "lon_lat")

  test_integrity(output2)
})


# test transform and subset method
test_that("test transform (space) method", {
  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  # Explicitly load grid
  output$transform(to = c("lon_lat"))
  output$subset(lat = c("55.25", "55.75", "56.25", "56.75"))
  output$transform(to = "cell")
  # test_integrity(output)

  output2 <- read_grid(
    filename = file_name,
    dim_order = c("band", "cell", "time")
  )

  output2$transform(to = c("lon_lat"))
  output2$subset(lat = c("55.25", "55.75", "56.25", "56.75"))
  output2$transform(to = "cell")
  test_integrity(output2)
  expect_identical(
    output2$data,
    aperm(output$data, names(dim(output2)))
  )
})


# coordinates located within the last two cells
coordinates <- list(
  lat = c("55.9", "63.7"),
  lon = c("-87.3", "-87.1")
)
coordinates_numeric <- list(
  lon = as.numeric(coordinates$lon),
  lat = as.numeric(coordinates$lat)
)

# test subset method for coordinates (pair)
test_that("test transform (space) method", {

  file_name <- "../testdata/output/grid.bin.json"
  output <- read_grid(filename = file_name)

  output_trans <- transform(output, to = c("lon_lat"))
  output_trans$subset(coordinates = coordinates)
  output_back <- transform(output_trans, to = "cell")
  test_integrity(output_trans)
  test_integrity(output_back)
  if (!anyNA(output$data)) {
    expect_false(anyNA(output_trans$data))
  }

  output2 <- read_grid(
    filename = file_name,
    dim_order = c("band", "cell", "time")
  )

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
