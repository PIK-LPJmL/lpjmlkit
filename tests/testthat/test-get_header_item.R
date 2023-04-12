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
  nstep = 12,
  timestep = 10,
  endian = .Platform$endian,
  verbose = FALSE
)
test_that("get header item", {
  # Expect named vector of length 1 corresponding to the requested header item
  expect_equal(get_header_item(h1, "name"), c(name = "LPJGRID"))
  expect_equal(get_header_item(h1, "version"), c(version = 4))
  expect_equal(get_header_item(h1, "order"), c(order = 1))
  expect_equal(get_header_item(h1, "firstyear"), c(firstyear = 1901))
  expect_equal(get_header_item(h1, "nyear"), c(nyear = 1))
  expect_equal(get_header_item(h1, "firstcell"), c(firstcell = 0))
  expect_equal(get_header_item(h1, "ncell"), c(ncell = 67420))
  expect_equal(get_header_item(h1, "nbands"), c(nbands = 2))
  expect_equal(get_header_item(h1, "cellsize_lon"), c(cellsize_lon = 0.5))
  expect_equal(get_header_item(h1, "scalar"), c(scalar = 1.0))
  expect_equal(get_header_item(h1, "cellsize_lat"), c(cellsize_lat = 0.25))
  expect_equal(get_header_item(h1, "datatype"), c(datatype = 3))
  expect_equal(get_header_item(h1, "nstep"), c(nstep = 12))
  expect_equal(get_header_item(h1, "timestep"), c(timestep = 10))
  expect_equal(get_header_item(h1, "endian"), c(endian = .Platform$endian))
})

test_that("get_header_item error messages", {
  # Query more than 1 item
  expect_error(get_header_item(h1, c("name", "version")), "Invalid item")

  # Query non-existing item
  expect_error(get_header_item(h1, "hello"), "Invalid item")
})
