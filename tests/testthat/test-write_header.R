test_that("Write LPJmL file header", {

  # Temporary file used for writing tests.
  header_file <- tempfile("lpjmlkit")

  # Test for invalid headers.
  header <- "test"
  expect_error(
    write_header(header_file, header),
    "invalid structure.+list with elements"
  )

  header <- list(name = "LPJTEST")
  expect_error(
    write_header(header_file, header),
    "invalid structure.+list with elements"
  )

  header <- list(
    name = "LPJGRID",
    header = c(
      version = 4,
      order = NA,
      firstyear = 1901,
      nyear = 1,
      firstcell = 0,
      ncell = 67420,
      nbands = 2,
      cellsize_lon = 0.5,
      scalar = NA,
      cellsize_lat = 0.5,
      datatype = NA,
      nstep = NA,
      timestep = 10
    ),
    endian = .Platform$endian
  )
  # Test for error about order = NA
  expect_error(
    write_header(header_file, header),
    "NA.+order"
  )

  # Test for error about scalar.
  header[["header"]]["order"] <- 1
  expect_error(
    write_header(header_file, header),
    "NA.+scalar"
  )

  # Test for error about datatype.
  header[["header"]]["scalar"] <- 1
  expect_error(
    write_header(header_file, header),
    "NA.+datatype"
  )

  # Test for error about nstep
  header[["header"]]["datatype"] <- 1
  expect_error(
    write_header(header_file, header),
    "NA.+nstep"
  )

  # Test with valid header
  header <- create_header(
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
    timestep = 1,
    endian = .Platform$endian,
    verbose = FALSE
  )
  # Test that created file has expected size
  # Write header to file.
  expect_warning(
    write_header(header_file, header),
    NA
  )
  expect_equal(
    file.size(header_file),
    get_headersize(header),
    label = "Generated version-4 header file size",
    expected.label = "expected size given by get_headersize()"
  )

  # Test if attempting to overwrite existing file fails.
  expect_error(
    write_header(header_file, header),
    "exists already"
  )

  # Test if overwriting existing file can be done if setting overwrite to TRUE,
  # with warning
  expect_warning(
    write_header(header_file, header, overwrite = TRUE),
    "exists already"
  )

  # Change to version 3.
  header <- set_header_item(header, version = 3, verbose = FALSE)
  file.remove(header_file)
  expect_warning(
    write_header(header_file, header),
    NA
  )
  expect_equal(
    file.size(header_file),
    get_headersize(header),
    label = "Generated version-3 header file size",
    expected.label = "expected size given by get_headersize()"
  )

  # Change to version 2.
  header <- set_header_item(header, version = 2, verbose = FALSE)
  file.remove(header_file)
  expect_warning(
    write_header(header_file, header),
    NA
  )
  expect_equal(
    file.size(header_file),
    get_headersize(header),
    label = "Generated version-2 header file size",
    expected.label = "expected size given by get_headersize()"
  )

  # Change to version 1.
  header <- set_header_item(header, version = 1, verbose = FALSE)
  file.remove(header_file)
  expect_warning(
    write_header(header_file, header),
    NA
  )
  expect_equal(
    file.size(header_file),
    get_headersize(header),
    label = "Generated version-3 header file size",
    expected.label = "expected size given by get_headersize()"
  )

  # Final clean-up.
  file.remove(header_file)

})
