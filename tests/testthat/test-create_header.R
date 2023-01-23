test_that("Create LPJmL file header", {
h1 <- create_header(
  name = "LPJGRID",
  version = 3,
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
  timestep = 1,
  endian = .Platform$endian,
  verbose = FALSE
)
# Test that returned header has valid structure
expect_type(h1, "list")
expect_named(h1, c("name", "header", "endian"))
expect_length(h1$name, 1)
expect_length(h1$header, 13)
expect_length(h1$endian, 1)
})
