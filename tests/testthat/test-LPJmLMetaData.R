# test LPJmLMetaData content/ as_list function with JSON file content
test_that("test LPJmLMetaData content", {
  file_name <- "../testdata/output/pft_npp.bin.json"

  # read in json data via jsonlite library
  meta_list <- jsonlite::read_json(path = file_name, simplify = TRUE)

  # read in meta data to LPJmLMetaData object
  meta_data <- read_meta(
    filename = file_name
  ) %>%
    # export as list for comarability
    as_list() %>%
    # reorder for testing
    .[names(meta_list)]

  # test if meta_data information matches that of file
  expect_equal(meta_data, meta_list)
})


# test LPJmLMetaData as_header method with created header
test_that("test LPJmLMetaData as_header", {
  meta_header <- read_meta(
    filename = "../testdata/output/pft_npp.bin.json"
  ) %>%
    # export as header
    as_header(silent = TRUE)

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
  expect_equal(meta_header, test_header)
})

# Test LPJmLMetaData as_header method with created header
test_that("test read_meta & as_header", {

  # Read header as meta and convert to header
  meta_header <- read_meta(
    filename = "../testdata/header_v4.clm"
  )$as_header(silent = TRUE)

  # Directly read as header
  header <- read_header("../testdata/header_v4.clm")

  # Test for any information loss
  expect_equal(meta_header, header)

  # Test non_valid format
  expect_error(
    read_meta("../testdata/test_array_lonlat.rds"),
    "Non readable"
  )
})
