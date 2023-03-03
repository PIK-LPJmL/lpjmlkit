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
  testthat::expect_equal(
    dimnames_data$cell,
    format(
      seq(meta$firstcell, length.out = meta$ncell),
      trim = TRUE, scientific = FALSE
    )
  )

  # test for equal length of time steps in data and meta data (nyear * nstep)
  testthat::expect_equal(dim_data[["time"]], meta$nyear)
  # test for equal dimnames of time steps in data and those constructed by
  #   meta data with create_time_names function (nstep, firstyear, nyear)
  testthat::expect_equal(
    dimnames_data$time,
    create_time_names(
      meta$nstep,
      seq(meta$firstyear, by = meta$timestep, length.out = meta$nyear)
    )
  )

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


# test length method
test_that("test length method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  testthat::expect_equal(length(output), length(output$data))
})


# test dim method
test_that("test dim method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  testthat::expect_equal(dim(output), dim(output$data))
})


# test dimnames method
test_that("test dimnames method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  testthat::expect_equal(dimnames(output), dimnames(output$data))
})


# Test summary method
test_that("test summary method", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # For dimension time
  out_sum <- summary(output, dimension = "time")
  testthat::expect_equal(
    dimnames(output)$time,
    gsub(" ", "", attr(out_sum, "dimnames")[[2]])
  )

  out_sum <- summary(output)
  testthat::expect_match(
    out_sum[4],
    regexp = as.character(round(mean(output$data), digits = 1))
  )

  output <- read_io(filename = "../testdata/output/pft_npp.bin.json")
  out_sum <- summary(output, cutoff = TRUE)

  # For cutoff arg
  testthat::expect_equal(
    gsub(" ", "", dimnames(output)$band[1:16]),
    gsub(" ", "", attr(out_sum, "dimnames")[[2]])
  )

  # For subsets
  out_sum2 <- summary(
    output,
    subset = list(cell = 1, time = 1),
  )
  output_sub <- subset(output, cell = 1, time = 1)
  testthat::expect_equal(
    gsub(" ", "", dimnames(output_sub)$band),
    gsub(" ", "", attr(out_sum2, "dimnames")[[2]])
  )

  # For underlying grid
  output$add_grid()
  grid_sum <- summary(output$grid)
  testthat::expect_equal(
    dimnames(output$grid)$band,
    gsub(" ", "", attr(grid_sum, "dimnames")[[2]])
  )

  grid <- read_io(
    filename = "../testdata/output/grid.bin.json",
    subset = list(cell = 1),
    band_names = c("lon", "lat")
  )

  grid_sum <- summary(grid)
  testthat::expect_equal(
    dimnames(grid)$band,
    gsub(" ", "", attr(grid_sum, "dimnames")[[2]])
  )

})


test_that("test print method", {
  output <- read_io(filename = "../testdata/output/npp.bin.json")

  # Check if meta data printed
  testthat::expect_output(
    print(output),
    "meta"
  )
  testthat::expect_output(
    print(output),
    "subset"
  )

  # Check if data printed
  testthat::expect_output(
    print(output),
    "data"
  )
  testthat::expect_output(
    print(output),
    "time"
  )

  # Check if grid is added and printed
  output$add_grid()
  testthat::expect_output(
    print(output),
    "grid"
  )
})
