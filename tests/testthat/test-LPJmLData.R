# utility function to test data integrity
#   tests designed for data to still have sequential order (c(1,2,3) NOT c(1,3))
#   latter would not work with following simplified tests
test_integrity <- function(output, meta = NULL) {

  # check if meta data is provided additionally for testing meta data correctness # nolint
  if (!is.null(meta)) {
    # test if outputs meta data is equal to meta data (usually read in via
    #   read_meta function)
    testthat::expect_equal(output$meta, meta)
  } else {
    # else use outputs meta data
    meta <- output$meta
  }

  # do call dim and dimnames only once
  dim_data <- dim(output$data)
  dimnames_data <- dimnames(output$data)

  # test for equal length of cell in data and meta data (ncell)
  testthat::expect_equal(dim_data[["cell"]], meta$ncell)
  # test for equal dimnames of cell in data and those constructed by meta data
  testthat::expect_equal(dimnames_data$cell,
                         as.character(
                          seq(meta$firstcell, length.out = meta$ncell)
                         ))

  # test for equal length of time steps in data and meta data (nyear * nstep)
  testthat::expect_equal(dim_data[["time"]], meta$nyear)
  # test for equal dimnames of time steps in data and those constructed by
  #   meta data with create_time_names function (nstep, firstyear, nyear)
  testthat::expect_equal(dimnames_data$time,
                         create_time_names(meta$nstep,
                                           seq(meta$firstyear, length.out = meta$nyear))) # nolint

  # test for equal length of bands in data and meta data (nbands)
  testthat::expect_equal(dim_data[["band"]], meta$nbands)
  # check if band dimension > 1 -> then has band_names
  if (meta$nbands > 1) {
    # test for equal dimnames of band in data and those constructed by meta data
    #   (band_names)
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
  # read in grid directly and initialize object as LPJmLGridData
  grid <- read_io("../testdata/output/grid.bin.json") %>%
    LPJmLGridData$new()
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
