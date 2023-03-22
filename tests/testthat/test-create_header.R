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
testthat::expect_type(h1, "list")
testthat::expect_named(h1, c("name", "header", "endian"))
testthat::expect_length(h1$name, 1)
testthat::expect_length(h1$header, 13)
testthat::expect_length(h1$endian, 1)
})

test_that("Errors in create_header", {
  # Non-character name
  testthat::expect_error(
    create_header(name = 2),
    "must be a character vector"
  )
  # Floating point value for integer header attribute
  testthat::expect_error(
    create_header(version = 2.4),
    "must be an integer"
  )
  # Character value for integer header attribute
  testthat::expect_error(
    create_header(ncell = "0"),
    "must be an integer"
  )
  # Multiple values for same header attribute
  testthat::expect_error(
    create_header(order = c(1, 2)),
    "must be an integer"
  )

  # Header version 2 attributes
  testthat::expect_error(
    create_header(ncell = 2, cellsize_lon = c(0.5, 0.25)),
    "must be a float"
  )
  testthat::expect_error(
    create_header(ncell = 2, scalar = "0"),
    "must be a float"
  )
  # No error for version-2 attributes if version is 1 (defaults used instead)
  testthat::expect_error(
    create_header(version = 1, ncell = 2, cellsize_lon = c(0.5, 0.25)),
    NA
  )
  testthat::expect_error(
    create_header(version = 1, ncell = 2, scalar = "0"),
    NA
  )

  # Header version 3 attributes
  testthat::expect_error(
    create_header(ncell = 2, cellsize_lat = c(0.5, 0.25)),
    "must be a float"
  )
  testthat::expect_error(
    create_header(ncell = 2, cellsize_lat = "0"),
    "must be a float"
  )
  # No error for invalid cellsize_lat if version < 3
  testthat::expect_error(
    create_header(version = 2, ncell = 2, cellsize_lat = "0"),
    NA
  )
  testthat::expect_error(
    create_header(ncell = 2, datatype = 7),
    "Unknown datatype"
  )
  testthat::expect_error(
    create_header(ncell = 2, datatype = c(1, 3)),
    "must be an integer"
  )

  # Header version 4 attributes
  testthat::expect_error(
    create_header(version = 4, ncell = 2, nstep = 2.4, verbose = FALSE),
    "must be an integer"
  )
  testthat::expect_error(
    create_header(version = 4, ncell = 2, timestep = "0", verbose = FALSE),
    "must be an integer"
  )
  testthat::expect_error(
    create_header(version = 4, ncell = 2, nstep = c(1, 2), verbose = FALSE),
    "must be an integer"
  )
  # Do not raise errors for invalid version-4 attributes if version is below 4
  testthat::expect_error(
    create_header(version = 3, ncell = 2, nstep = 2.4, verbose = FALSE),
    NA
  )
  testthat::expect_error(
    create_header(version = 3, ncell = 2, timestep = c(1, 2), verbose = FALSE),
    NA
  )

  # Invalid endian
  testthat::expect_error(
    create_header(ncell = 2, endian = "test", verbose = FALSE),
    "Endian must be"
  )
})

test_that("Warnings in create_header", {
  # Invalid header name
  testthat::expect_warning(
    create_header("NONAME", ncell = 1),
    "Header name.+is probably invalid"
  ) %>% testthat::expect_message("Setting datatype")
  testthat::expect_warning(
    create_header(version = 1, ncell = 1, cellsize_lon = 0.75),
    "Setting non-default cellsize_lon"
  )
  testthat::expect_warning(
    create_header(version = 1, ncell = 1, scalar = 0.75),
    "Setting non-default scalar"
  )
  testthat::expect_warning(
    create_header(version = 2, ncell = 1, cellsize_lat = 0.75),
    "Setting non-default cellsize_lat"
  )
  testthat::expect_warning(
    create_header(version = 2, ncell = 1, datatype = 3),
    "Setting non-default datatype"
  )
  testthat::expect_warning(
    create_header(version = 3, ncell = 1, nstep = 12),
    "Setting non-default nstep"
  ) %>% testthat::expect_message("Setting datatype")
  testthat::expect_warning(
    create_header(version = 3, ncell = 1, timestep = 5),
    "Setting non-default timestep"
  ) %>% testthat::expect_message("Setting datatype")
})

test_that("Tests for is_valid_header", {
  # List
  testthat::expect_error(is_valid_header("test"), "invalid structure.*list")
  # List with correct elements
  testthat::expect_error(
    is_valid_header(list(name = "test")),
    "invalid structure.*list"
  )

  # No more than one name
  header <- list(
    name = c("Hello", "world"),
    header = integer(13),
    endian = "little"
  )
  testthat::expect_error(
    is_valid_header(header),
    "invalid structure. More than one"
  )

  # Invalid element in header list
  header <- list(
    name = "LPJGRID",
    header = integer(13),
    endian = "little",
    invalid = "item"
  )
  testthat::expect_error(
    is_valid_header(header),
    "invalid structure.*list"
  )

  # Wrong number of header$header elements
  header <- list(
    name = c("LPJGRID"),
    header = integer(12),
    endian = "little"
  )
  testthat::expect_error(
    is_valid_header(header),
    "invalid structure. Invalid header\\$header"
  )

  # All expected items in header$header
  header <- list(
    name = c("LPJGRID"),
    header = seq_len(13),
    endian = "little"
  )
  # Set correct names
  names(header$header) <- setdiff(valid_header_items, names(header))
  # Set one name wrong
  names(header$header)[1] <- "random"
  testthat::expect_error(
    is_valid_header(header),
    "invalid structure: item\\(s\\)"
  )

  # NA in elements
  names(header$header) <- setdiff(valid_header_items, names(header))
  header$header["version"] <- NA
  testthat::expect_error(
    is_valid_header(header),
    "Header values must not be set to NA."
  )
})
