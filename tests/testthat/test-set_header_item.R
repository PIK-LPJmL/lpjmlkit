# Dummy header
h1 <- create_header(
  name = "LPJGRID",
  version = 4,
  order = 1,
  firstyear = 1901,
  nyear = 1,
  firstcell = 0,
  ncell = 67420,
  nbands = 2,
  cellsize_lon = 0.5,
  scalar = 1,
  cellsize_lat = 0.25,
  datatype = 3,
  nstep = 1,
  timestep = 10,
  endian = .Platform$endian,
  verbose = FALSE
)

test_that("set header item", {
  # set_header_item should always return a correct header where header item(s)
  # provided to function should be updated whereas all other values are kept
  # Test changing individual items
  testthat::expect_message(
    h2 <- set_header_item(h1, name = "LPJCLIM"),
    "Setting datatype"
  )
  # Test that returned header is valid
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$name, "LPJCLIM")
  # Re-setting version can result in a warning if parameters are non-default
  # with new version.
  testthat::expect_warning(
    h2 <- set_header_item(h1, version = 2),
    "Setting.+non-default.+"
  )
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["version"], c(version = 2))
  # Set order
  h2 <- set_header_item(h1, order = 2)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["order"], c(order = 2))
  # Set firstyear
  h2 <- set_header_item(h1, firstyear = 1)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["firstyear"], c(firstyear = 1))
  # Set nyear
  h2 <- set_header_item(h1, nyear = 10)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["nyear"], c(nyear = 10))
  # Set firstcell
  h2 <- set_header_item(h1, firstcell = 1)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["firstcell"], c(firstcell = 1))
  # Set ncell
  h2 <- set_header_item(h1, ncell = 1)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["ncell"], c(ncell = 1))
  # Set nbands
  h2 <- set_header_item(h1, nbands = 1)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["nbands"], c(nbands = 1))
  # Set cellsize_lon
  h2 <- set_header_item(h1, cellsize_lon = 1)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["cellsize_lon"], c(cellsize_lon = 1))
  # Set scalar
  h2 <- set_header_item(h1, scalar = 0.01)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["scalar"], c(scalar = 0.01))
  # Set cellsize_lat
  h2 <- set_header_item(h1, cellsize_lat = 2.5)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["cellsize_lat"], c(cellsize_lat = 2.5))
  # Setting datatype can result in a warning, suppress
  testthat::expect_message(
    h2 <- set_header_item(h1, datatype = 4),
    "Setting datatype"
  )
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["datatype"], c(datatype = 4))
  # Set nstep
  h2 <- set_header_item(h1, nstep = 12)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["nstep"], c(nstep = 12))
  # Set timestep
  h2 <- set_header_item(h1, timestep = 5)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$header["timestep"], c(timestep = 5))
  # Set endian
  h2 <- set_header_item(h1, endian = "big")
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  testthat::expect_equal(h2$endian, "big")

  # Try to set more than one item at once (should work)
  h2 <- set_header_item(h1, ncell = 6, nstep = 12)
  testthat::expect_error(is_valid_header(h2), NA)
  # Test updated value
  expect_equal(h2$header["ncell"], c(ncell = 6))
  expect_equal(h2$header["nstep"], c(nstep = 12))
})

test_that("set_header_item error messages", {
  # Check with invalid header
  testthat::expect_error(
    set_header_item(h1$header, name = "LPJCLIM"),
    "invalid.*structure"
  )
  # Try to set item that does not exist
  testthat::expect_error(
    set_header_item(h1, hello = "world"),
    "invalid.*item", ignore.case = TRUE
  )
  # Try to set same item twice
  testthat::expect_error(
    set_header_item(h1, version = 2, version = 3),
    "more.+than.+once"
  )

  # Try to supply item with length != 1
  testthat::expect_error(
    set_header_item(h1, nstep = c(1, 12)),
    "more than one value"
  )
})
