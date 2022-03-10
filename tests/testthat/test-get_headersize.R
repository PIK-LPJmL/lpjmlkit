test_that("get_headersize returns correct header size", {
  # Test files contain only the header so header size should equal file size.
  # Version 1 header
  # Version 1 headers return a warning on read about default parameters so
  # suppress warning.
  h1 <- suppressWarnings(read_header("../testdata/header_v1.clm"))
  expect_identical(
    get_headersize(h1),
    file.size("../testdata/header_v1.clm")
  )
  # Version 2 header
  # Version 2 headers return a warning on read about default parameters so
  # suppress warning.
  h2 <- suppressWarnings(read_header("../testdata/header_v2.clm"))
  expect_identical(
    get_headersize(h2),
    file.size("../testdata/header_v2.clm")
  )
  # Version 3 header
  # Version 3 headers return a warning on read about default parameters so
  # suppress warning.
  h3 <- suppressWarnings(read_header("../testdata/header_v3.clm"))
  expect_identical(
    get_headersize(h3),
    file.size("../testdata/header_v3.clm")
  )
  # Version 4 header
  # Version 4 headers have all parameters so no warning should be given.
  h4 <- read_header("../testdata/header_v4.clm")
  expect_identical(
    get_headersize(h4),
    file.size("../testdata/header_v4.clm")
  )
})

test_that("get_headersize error messages", {
  # get_headersize expects a list, try just passing a vector
  h4 <- c(version = 2, ncell = 67420)
  expect_error(get_headersize(h4), "must be a list")

  # Try giving a list with wrong elements
  h1 <- suppressWarnings(read_header("../testdata/header_v1.clm"))
  h5 <- h1
  h5[["holiday"]] <- h5[["name"]]
  h5[["name"]] <- NULL
  expect_error(get_headersize(h5), "list.*name.*header")
  
  # Try giving invalid header version
  h6 <- h1
  h6[["header"]]["version"] <- 5
  expect_error(get_headersize(h6), "invalid header version", ignore.case = TRUE)
})
