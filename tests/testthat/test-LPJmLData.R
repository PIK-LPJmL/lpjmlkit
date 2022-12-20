# utility function to test data integrity
test_integrity <- function(output, meta = NULL) {

  if (!is.null(meta)) {
    # check for meta data
    testthat::expect_equal(output$meta, meta)
  } else {
    meta <- output$meta
  }

  # check for data
  dim_data <- dim(output$data)
  dimnames_data <- dimnames(output$data)
  #   check for cell dimension
  testthat::expect_equal(dim_data[["cell"]], meta$ncell)
  testthat::expect_equal(dimnames_data$cell,
                         as.character(
                          seq(meta$firstcell, length.out = meta$ncell)
                         ))
  #   check for time dimension
  testthat::expect_equal(dim_data[["time"]], meta$nyear)
  testthat::expect_equal(dimnames_data$time,
                         create_time_names(meta$nstep,
                                           seq(meta$firstyear, length.out = meta$nyear))) # nolint
  #   check for band dimension
  testthat::expect_equal(dim_data[["band"]], meta$nbands)
  if (meta$nbands > 1) {
    testthat::expect_equal(dimnames_data$band, meta$band_names)
  }

}


# test composition and integrity for basic 1 band output
test_that("composition basic 1 band output", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  meta <- read_meta(filename = file_name)

  test_integrity(output, meta)
})


# test composition and integrity for basic multiple band output
test_that("composition basic multiple band output", {
  file_name <- "../testdata/output/pft_npp.bin.json"
  output <- read_io(filename = file_name)
  meta <- read_meta(filename = file_name)

  test_integrity(output, meta)
})


# test add_grid method
test_that("test add_grid method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  # perform adding a grid object
  output$add_grid()
  # read in grid directly
  grid <- read_io("../testdata/output/grid.bin.json")
  # check if added grid equals grid file (read in separately)
  expect_equal(output$grid, grid)
  # check grid cells with those of objects meta data
  expect_equal(dimnames(output$grid$data)$cell,
               as.character(
                seq(output$meta$firstcell, length.out = output$meta$ncell)
               ))
})

# test length method
test_that("test length method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  expect_equal(length(output), length(output$data))
})


# test dim method
test_that("test dim method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  expect_equal(dim(output), dim(output$data))
})


# test dimnames method
test_that("test dimnames method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  expect_equal(dimnames(output), dimnames(output$data))
})
