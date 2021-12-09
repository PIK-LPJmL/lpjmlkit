test_that("get_datatype return value is of expected type", {
  # Dummy minimum viable header
  h1 <- c(datatype = 1)
  # Function should return a list with elements "type", "size" and "signed"
  expect_type(get_datatype(h1), "list")
  expect_named(get_datatype(h1), c("type", "size", "signed"))
  
  # Dummy header with normal header structure
  h2 <- list(
    name = "LPJGRID",
    header = c(
      version = 1,
      order = 1,
      firstyear = 1901,
      nyear = 1,
      firstcell = 0,
      ncell = 67420,
      nbands = 1,
      cellsize_lon = 0.5,
      scalar = 1.0,
      cellsize_lat = 0.5,
      datatype = 3
    ),
    endian = .Platform$endian
  )
  expect_type(get_datatype(h2), "list")
  expect_named(get_datatype(h2), c("type", "size", "signed"))
})

test_that("get_datatype error messages", {
  # Missing datatype entry should give an error
  # Dummy header
  h1 <- c(version = 2)
  expect_error(get_datatype(h1), "contain.*datatype")

  # Invalid datatype should give an error
  h2 <- c(datatype = -1)
  expect_error(get_datatype(h2), "invalid datatype", ignore.case = TRUE)
})
