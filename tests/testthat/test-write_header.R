test_that("Write LPJmL file header", {
h4 <- create_header(
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
  cellsize_lat = 0.5,
  datatype = 1,
  nstep = 1,
  timestep = 10,
  endian = .Platform$endian,
  verbose = FALSE
)
# Test that created file has expected size
# Write header to file. Suppress possible warning if generated files exists.
suppressWarnings(
  write_header("../testdata/header_test.clm", h4, overwrite = TRUE)
)
expect_equal(
  file.size("../testdata/header_test.clm"),
  get_headersize(h4),
  label = "Generated version-4 header file size",
  expected.label = "expected size given by get_headersize()"
)
# Change to version 3
suppressWarnings(h3 <- set_header_item(h4, version = 3))
suppressWarnings(
  write_header("../testdata/header_test.clm", h3, overwrite = TRUE)
)
expect_equal(
  file.size("../testdata/header_test.clm"),
  get_headersize(h3),
  label = "Generated version-3 header file size",
  expected.label = "expected size given by get_headersize()"
)
# Change to version 2
suppressWarnings(h2 <- set_header_item(h4, version = 2))
suppressWarnings(
  write_header("../testdata/header_test.clm", h2, overwrite = TRUE)
)
expect_equal(
  file.size("../testdata/header_test.clm"),
  get_headersize(h2),
  label = "Generated version-2 header file size",
  expected.label = "expected size given by get_headersize()"
)
# Change to version 1
suppressWarnings(h1 <- set_header_item(h4, version = 1))
suppressWarnings(
  write_header("../testdata/header_test.clm", h1, overwrite = TRUE)
)
expect_equal(
  file.size("../testdata/header_test.clm"),
  get_headersize(h1),
  label = "Generated version-1 header file size",
  expected.label = "expected size given by get_headersize()"
)

})
