# Dummy header
h1 <- list(
  name = "LPJGRID",
  header = c(
    version = 2,
    order = 1,
    firstyear = 1901,
    nyear = 1,
    firstcell = 0,
    ncell = 67420,
    nbands = 2,
    cellsize_lon = 0.5,
    scalar = 1,
    cellsize_lat = 0.25,
    datatype = 3
  ),
  endian = .Platform$endian
)

test_that("set header item", {
  # set_header_item should always return a correct header where header item(s)
  # provided to function should be updated whereas all other values are kept
  # Test changing individual items
  # Setting name can result in a warning, suppress
  h2 <- suppressWarnings(set_header_item(h1, name = "LPJCLIM"))
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$name, "LPJCLIM", ignore_attr = TRUE)
  # Setting version can result in a warning, suppress
  h2 <- suppressWarnings(set_header_item(h1, version = 2))
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["version"], 2, ignore_attr = TRUE)
  # Set order
  h2 <- set_header_item(h1, order = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["order"], 1, ignore_attr = TRUE)
  # Set firstyear
  h2 <- set_header_item(h1, firstyear = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["firstyear"], 1, ignore_attr = TRUE)
  # Set nyear
  h2 <- set_header_item(h1, nyear = 10)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["nyear"], 10, ignore_attr = TRUE)
  # Set firstcell
  h2 <- set_header_item(h1, firstcell = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["firstcell"], 1, ignore_attr = TRUE)
  # Set ncell
  h2 <- set_header_item(h1, ncell = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["ncell"], 1, ignore_attr = TRUE)
  # Set nbands
  h2 <- set_header_item(h1, nbands = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["nbands"], 1, ignore_attr = TRUE)
  # Set cellsize_lon
  h2 <- set_header_item(h1, cellsize_lon = 1)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["cellsize_lon"], 1, ignore_attr = TRUE)
  # Set scalar
  h2 <- set_header_item(h1, scalar = 0.01)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["scalar"], 0.01, ignore_attr = TRUE)
  # Set cellsize_lat
  h2 <- set_header_item(h1, cellsize_lat = 2.5)
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["cellsize_lat"], 2.5, ignore_attr = TRUE)
  # Setting datatype can result in a warning, suppress
  h2 <- suppressWarnings(set_header_item(h1, datatype = 4))
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$header["datatype"], 4, ignore_attr = TRUE)
  # Set endian
  h2 <- set_header_item(h1, endian = "big")
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$endian, "big", ignore_attr = TRUE)

  # Try to set more than one item at once (should work)
  h2 <- suppressWarnings(set_header_item(h1, name = "LPJCLIM", version = 2))
  expect_type(h2, "list")
  expect_named(h2, c("name", "header", "endian"))
  # Test updated value
  expect_equal(h2$name, "LPJCLIM", ignore_attr = TRUE)
  expect_equal(h2$header["version"], 2, ignore_attr = TRUE)
})

test_that("set_header_item error messages", {
  # Check with invalid header
  expect_error(
    set_header_item(h1$header, name = "LPJCLIM"),
    "invalid.*structure"
  )
  # Try to set item that does not exist
  expect_error(
    set_header_item(h1, hello = "world"),
    "invalid.*item", ignore.case = TRUE
  )
  # Try to set same item twice
  expect_error(
    set_header_item(h1, version = 2, version = 3),
    "more.*than.*once"
  )
})
