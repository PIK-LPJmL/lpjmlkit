test_that("read in LPJmL file header version 1", {
  # Version 1 headers give a warning, supress
  header <- suppressWarnings(read_header("../testdata/header_v1.clm"))
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
  # Test that message about default parameters is given
  expect_message(
    read_header(
      "../testdata/header_v1.clm",
      verbose = TRUE
    ),
    "Type 1 header.*cellsize.*scalar"
  )
})

test_that("read in LPJmL file header version 2", {
  # Version 2 headers give a warning, suppress
  header <- suppressWarnings(read_header("../testdata/header_v2.clm"))
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
  # Test that message about default parameters is given
  expect_message(
    read_header(
      "../testdata/header_v2.clm",
      verbose = TRUE
    ),
    "Type 2 header.*datatype"
  )
})

test_that("read in LPJmL file header version 3", {
  # Version 3 headers give a warning, suppress
  header <- suppressWarnings(read_header("../testdata/header_v3.clm"))
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
  # Test that message about default parameters is given
  expect_message(
    read_header(
      "../testdata/header_v3.clm",
      verbose = TRUE
    ),
    "Type 3 header.*nstep"
  )
})

test_that("read in LPJmL file header version 4", {
  header <- read_header("../testdata/header_v4.clm")
  # Test that header was loaded
  expect_true(exists("header"))
  # Test that header is a list
  expect_type(header, "list")
  # Test that header has all expected elements
  expect_named(header, c("name", "header", "endian"))
  # Test that no warning about default parameters is given because version 4
  # has all values
  expect_warning(
    read_header(
      "../testdata/header_v4.clm",
      verbose = TRUE
    ),
    NA
  )
})

# Test that missing file gives expected error message
test_that("try to read non-existing file header", {
  expect_error(
    read_header("../testdata/no_file"),
    "does not exist"
  )
})
