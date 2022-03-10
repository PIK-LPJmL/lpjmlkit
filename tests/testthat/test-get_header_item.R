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
    nstep = 12,
    datatype = 3
  ),
  endian = .Platform$endian
)
test_that("get header item", {
  # Expect named vector of length 1 corresponding to the requested header item
  expect_equal(get_header_item(h1, "name"), c(name = "LPJGRID"))
  expect_equal(get_header_item(h1, "version"), c(version = 2))
  expect_equal(get_header_item(h1, "order"), c(order = 1))
  expect_equal(get_header_item(h1, "firstyear"), c(firstyear = 1901))
  expect_equal(get_header_item(h1, "nyear"), c(nyear = 1))
  expect_equal(get_header_item(h1, "firstcell"), c(firstcell = 0))
  expect_equal(get_header_item(h1, "ncell"), c(ncell = 67420))
  expect_equal(get_header_item(h1, "nbands"), c(nbands = 2))
  expect_equal(get_header_item(h1, "cellsize_lon"), c(cellsize_lon = 0.5))
  expect_equal(get_header_item(h1, "scalar"), c(scalar = 1.0))
  expect_equal(get_header_item(h1, "cellsize_lat"), c(cellsize_lat= 0.25))
  expect_equal(get_header_item(h1, "datatype"), c(datatype = 3))
  expect_equal(get_header_item(h1, "nstep"), c(nstep = 12))
  expect_equal(get_header_item(h1, "endian"), c(endian = .Platform$endian))
})

test_that("get_header_item error messages", {
  # Invalid header structure
  # No list
  expect_error(get_header_item(h1$header, "name"), "invalid.*structure")
  # Missing name attribute
  expect_error(get_header_item(h1[-1], "name"), "invalid.*structure")
  # Missing element in header$header
  h2 <- h1
  h2$header <- h2$header[-4]
  expect_error(get_header_item(h2, "name"), "invalid.*structure")
  # Incorrectly named elements in header$header
  h2 <- h1
  names(h2$header)[1] <- "total"
  expect_error(get_header_item(h2, "version"), "invalid.*structure.*is missing")
  # item that does not exist
  expect_error(
    get_header_item(h1, "hello"),
    "invalid.*item", ignore.case = TRUE
  )
  # It is not possible to query more than 1 item at a time
  expect_error(
    get_header_item(h1, c("name", "version")),
    "invalid.*item", ignore.case = TRUE
  )
})
