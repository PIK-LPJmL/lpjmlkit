test_that("Read in LPJmL file header version 1", {
  # Version 1 headers give a message about default parameters being used.
  expect_message(
    read_header("../testdata/header_v1.clm"),
    "Type 1 header.*cellsize.*scalar"
  )

  # Message can be suppressed by setting verbose to FALSE.
  header <- read_header("../testdata/header_v1.clm", verbose = FALSE)
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))

  # Try reading corresponding version 1 header with big endian.
  header2 <- read_header("../testdata/header_v1_BE.clm", verbose = FALSE)

  # Headers should be identical except for endian
  expect_equal(
    header[c("name", "header")], header2[c("name", "header")]
  )
  expect_equal(header2$endian, "big")
})

test_that("Read in LPJmL file header version 2", {
  # Version 2 headers give a message about default parameters being used.
  expect_message(
    read_header("../testdata/header_v2.clm"),
    "Type 2 header.*datatype"
  )

  header <- read_header("../testdata/header_v2.clm", verbose = FALSE)
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
  # Test that message about default parameters is given
})

test_that("Read in LPJmL file header version 3", {
  # Version 3 headers give a message about default parameters being used.
  expect_message(
    read_header("../testdata/header_v3.clm"),
    "Type 3 header.*nstep"
  )

  header <- read_header("../testdata/header_v3.clm", verbose = FALSE)
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
})

test_that("Read in LPJmL file header version 3 with invalid datatype", {
  # Version 3 headers give a message about default parameters being used.
  # Also expect warning for invalid datatype.
  expect_warning(
    expect_message(
      read_header("../testdata/header_v3_dt.clm"),
      "Type 3 header.*nstep"
    ),
    "Invalid datatype"
  )
})

test_that("Read in LPJmL file header version 4", {
  # Version 4 header give no message about default parameters being used.
  expect_message(
    read_header("../testdata/header_v4.clm"),
    NA
  )
  header <- read_header("../testdata/header_v4.clm")
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))

  # Force version to 3.
  expect_message(
    expect_message(
      header2 <- read_header("../testdata/header_v4.clm", force_version = 3),
      "Forcing header version"
    ),
    "Type 3 header.*nstep"
  )

  # Version 3 does not support timestep in header.
  expect_true(
    header$header[["timestep"]] != header2$header["timestep"]
  )
})

# Test that missing file gives expected error message
test_that("Try to read non-existing file header", {
  expect_error(
    read_header("../testdata/no_file"),
    "does not exist"
  )
})

test_that("Try to read invalid header names", {
  expect_error(
    read_header("../testdata/header_restart.clm"),
    "LPJRESTART header detected"
  )

  expect_error(
    read_header("../testdata/header_invalid.clm"),
    "Invalid header name"
  )
})
