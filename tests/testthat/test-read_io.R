# outputs in RAW format
test_that("read io - .bin - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.bin"
  out <- read_io(
    filename = out_filename,
    file_type = "raw",
    nbands = 43,
    firstcell = 10000,
    ncell = 3,
    firstyear = 2001,
    nyear = 11
  )
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

# outputs in CLM format
test_that("read io - .clm - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.clm"
  out <- read_io(filename = out_filename, file_type = "clm")
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
  # Force version
  out <- read_io(filename = out_filename, file_type = "clm", version = 4)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

# ------------------------------------ #
# outputs in META format & different time steps
test_that("read io - .bin.json - daily", {
  out_filename <- "../testdata/output/d_lai.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

test_that("read io - .bin.json - monthly", {
  out_filename <- "../testdata/output/transp.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

test_that("read io - .bin.json - annual", {
  out_filename <- "../testdata/output/npp.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

test_that("read io - .bin.json - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

test_that("read_io errors", {
  # Non-existing file
  expect_error(
    read_io(tempfile()),
    "File.+does not exist"
  )
  # Unsupported file_type
  expect_error(
    read_io(tempfile(), file_type = "ghru"),
    "file_type.*is not supported"
  )
  # Invalid dimension names for dim_order
  expect_error(
    read_io(tempfile(), file_type = "clm", dim_order = c("test", "band")),
    "Invalid dim_order"
  )
  # Not all dimension names for dim_order
  expect_error(
    read_io(tempfile(), file_type = "clm", dim_order = c("cell", "band")),
    "Invalid dim_order"
  )
  # Empty year subset
  expect_error(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(year = NA),
      silent = TRUE
    ),
    "is empty after removal of NAs"
  )

  # Invalid band_names (number does not match number of bands)
  expect_error(
    read_io("../testdata/output/pft_npp.clm", band_names = "test"),
    "Provided band_names.+do not match number of bands in file"
  )
  expect_error(
    read_io(
      "../testdata/output/pft_npp.clm",
      band_names = as.character(seq_len(8))
    ),
    "Provided band_names.+do not match number of bands in file"
  )

  # Invalid years
  expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = 25)),
    "Year.*outside of file range"
  )
  expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = "125")),
    "Year.*outside of file range"
  )
  expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = TRUE)),
    "Unsupported type.+provided as subset"
  )
})

test_that("read_io warnings", {
  # NA values in subset
  expect_warning(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(cell = c(1, NA))
    ),
    "Removing NA values"
  )
  expect_warning(
    expect_warning(
      read_io(
        "../testdata/output/pft_npp.bin.json",
        subset = list(cell = c(NA))
      ),
      "empty after removal of NAs"
    ),
    "Removing NA values"
  )

  # Invalid order 0
  # Create temporary dummy file from existing grid file
  grid <- read_io("../testdata/output/grid.bin.json")
  header <- as_header(grid$meta, silent = TRUE)
  header <- set_header_item(
    header,
    name = "LPJGRID", order = 0, verbose = FALSE
  )
  tmp_filename <- tempfile("lpjmlkit")
  write_header(tmp_filename, header)
  file.append(tmp_filename, "../testdata/output/grid.bin")
  expect_warning(
    read_io(tmp_filename),
    "Header in file.*has invalid order"
  )
  file.remove(tmp_filename)

  # Attempt to overwrite name in clm file
  expect_warning(
    read_io("../testdata/output/pft_npp.clm", name = "LPJDUMMY"),
    "You cannot overwrite the header name in clm files"
  )

  # Duplicated year subset
  expect_warning(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(year = c("2001", "2001"))
    ),
    "Removing.+duplicate.+entr"
  )
  expect_warning(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(year = c(5, 5))
    ),
    "Removing.+duplicate.+entr"
  )
})

test_that("read_io messages", {
  # nbands representing nstep in header version <4
  h1 <- create_header(
    version = 2,
    ncell = 1,
    nbands = 12,
    datatype = 1,
    nyear = 1,
    verbose = FALSE
  )
  tmp_filename <- tempfile("lpjmlkit")
  write_header(tmp_filename, h1)
  tmp_file <- file(tmp_filename, "ab")
  writeBin(integer(12), tmp_file, size = get_datatype(h1)$size)
  close(tmp_file)
  expect_message(
    expect_message(
      read_io(tmp_filename),
      "Detected.+nbands.*consider setting"
    ),
    "Adding default values for"
  )
  file.remove(tmp_filename)
})

test_that("read_io for reservoir file",  {
  # Read in dummy reservoir file should be successful
  expect_message(
    testres <- read_io("../testdata/input/reservoir.bin"),
    "Reservoir file detected"
  )

  # Grid file cannot be read as reservoir
  expect_error(
    read_io("../testdata/output/grid.bin.json", name = "LPJDAMS"),
    "Expected size"
  )
})
