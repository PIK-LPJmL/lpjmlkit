# test LPJmLMetaData content/ as_list function with JSON file content
test_that("test LPJmLMetaData content", {
  # read in meta data to LPJmLMetaData object
  meta_data <- read_meta(
    filename = "../testdata/output/pft_npp.bin.json"
  ) %>%
    # export as list for comarability
    as_list() %>%
    # reorder for testing
    .[names(meta_list)]

  # read in json data via jsonlite library
  meta_list <- jsonlite::read_json(path = file_name, simplify = TRUE)
  # test if meta_data information matches that of file
  testthat::expect_equal(meta_data, meta_list)
})


# test LPJmLMetaData as_header method with created header
test_that("test LPJmLMetaData as_header", {
  meta_header <- read_meta(
    filename = "../testdata/output/pft_npp.bin.json"
  ) %>%
    # export as header
    as_header()

  # create header object
  test_header <- create_header(
      name = "LPJDUMMY",
      version = 4,
      order = 4,
      firstyear = 2001,
      nyear = 11,
      firstcell = 10000,
      ncell = 3,
      nbands = 43,
      cellsize_lon = 0.5,
      cellsize_lat = 0.5,
      datatype = 3,
      nstep = 1,
      timestep = 1,
      verbose = FALSE
  )

  # test if meta_data header export matches that of created header
  testthat::expect_equal(meta_header, test_header)
})
