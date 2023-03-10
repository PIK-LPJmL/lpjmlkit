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
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))
})

# outputs in CLM format
test_that("read io - .clm - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.clm"
  out <- read_io(filename = out_filename, file_type = "clm")
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))
  # Force version
  out <- read_io(filename = out_filename, file_type = "clm", version = 4)
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  testthat::expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  testthat::expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

# ------------------------------------ #
# outputs in META format & different time steps
test_that("read io - .bin.json - daily", {
  out_filename <- "../testdata/output/d_lai.bin.json"
  out <- read_io(filename = out_filename)
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  testthat::expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  testthat::expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

test_that("read io - .bin.json - monthly", {
  out_filename <- "../testdata/output/transp.bin.json"
  out <- read_io(filename = out_filename)
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))

  # Alternative dim_order
  out2 <- read_io(
    filename = out_filename,
    dim_order = c("band", "cell", "time")
  )
  testthat::expect_identical(
    out2$data,
    aperm(out$data, names(dim(out2)))
  )
  out3 <- read_io(
    filename = out_filename,
    dim_order = c("time", "band", "cell")
  )
  testthat::expect_identical(
    out3$data,
    aperm(out$data, names(dim(out3)))
  )
})

test_that("read io - .bin.json - annual", {
  out_filename <- "../testdata/output/npp.bin.json"
  out <- read_io(filename = out_filename)
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))
})

test_that("read io - .bin.json - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.bin.json"
  out <- read_io(filename = out_filename)
  testthat::expect_true(exists("out"))
  testthat::expect_true("LPJmLData" %in% class(out))
})

test_that("read_io errors", {
  # Non-existing file
  testthat::expect_error(
    read_io(tempfile()),
    "File.+does not exist"
  )
  # Unsupported file_type
  testthat::expect_error(
    read_io(tempfile(), file_type = "ghru"),
    "file_type.*is not supported"
  )
  # Invalid dimension names for dim_order
  testthat::expect_error(
    read_io(tempfile(), file_type = "clm", dim_order = c("test", "band")),
    "Invalid dim_order"
  )
  # Not all dimension names for dim_order
  testthat::expect_error(
    read_io(tempfile(), file_type = "clm", dim_order = c("cell", "band")),
    "Invalid dim_order"
  )
  # Empty year subset
  testthat::expect_error(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(year = NA),
      silent = TRUE
    ),
    "is empty after removal of NAs"
  )
  # Wrong file size
  # Create temporary file with wrong size
  tmp_filename <- tempfile("lpjmlkit")
  file.copy("../testdata/output/pft_npp.clm", tmp_filename)
  tmp_file <- file(tmp_filename, "ab")
  writeBin(4, tmp_file)
  close(tmp_file)
  testthat::expect_error(
    read_io(tmp_filename),
    "Unexpected file size"
  )
  file.remove(tmp_filename)

  # Invalid band_names (number does not match number of bands)
  testthat::expect_error(
    read_io("../testdata/output/pft_npp.clm", band_names = "test"),
    "Provided band_names.+do not match number of bands in file"
  )
  testthat::expect_error(
    read_io(
      "../testdata/output/pft_npp.clm",
      band_names = as.character(seq_len(8))
    ),
    "Provided band_names.+do not match number of bands in file"
  )
  # Unsupported LPJDAMS file
  header <- read_header("../testdata/header_v4.clm")
  header <- set_header_item(header, name = "LPJDAMS", verbose = FALSE)
  write_header(tmp_filename, header)
  testthat::expect_error(
    read_io(tmp_filename),
    "does not support reading LPJDAMS"
  )
  file.remove(tmp_filename)

  # Invalid years
  testthat::expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = 25)),
    "Year.*outside of file range"
  )
  testthat::expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = "125")),
    "Year.*outside of file range"
  )
  testthat::expect_error(
    read_io("../testdata/output/pft_npp.bin.json", subset = list(year = TRUE)),
    "Unsupported type.+provided as subset"
  )

  # Missing format in meta file
  tmp_filename <- tempfile("lpjmlkit")
  writeLines("{ \"sim_name\" : \"Test\" }", tmp_filename)
  testthat::expect_error(
    read_io(tmp_filename),
    "Missing 'format' in meta file"
  )

  # Unsupported format in meta file
  writeLines("{ \"format\" : \"cdf\" }", tmp_filename)
  testthat::expect_error(
    read_io(tmp_filename),
    "Format.+specified in meta file.*not supported"
  )
  file.remove(tmp_filename)

  # Missing linked file
  tmp_dirname <- tempfile("output")
  dir.create(tmp_dirname, recursive = TRUE)
  file.copy(
    "../testdata/output/grid.bin.json",
    file.path(tmp_dirname, "grid.bin.json")
  )
  testthat::expect_error(
    read_io(file.path(tmp_dirname, "grid.bin.json")),
    "File.*linked in meta file does not exist"
  )
  file.remove(file.path(tmp_dirname, "grid.bin.json"))

  # Relative path to linked file
  meta_list <- as_list(read_meta("../testdata/output/grid.bin.json"))
  tmp_filename1 <- tempfile("lpjmlkit")
  file.copy("../testdata/output/grid.bin", tmp_filename1)
  tmp_filename2 <- tempfile("lpjmlkit", tmpdir = tmp_dirname)
  meta_list$filename <- file.path("..", basename(tmp_filename1))
  jsonlite::write_json(
    x = meta_list,
    path = tmp_filename2,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = 10,
    always_decimal = TRUE
  )
  # Relative path to linked file is recognized, no error
  testthat::expect_error(
    output1 <- read_io(tmp_filename2),
    NA
  )
  file.remove(tmp_filename1, tmp_filename2)
  file.remove(tmp_dirname)

  output2 <- read_io("../testdata/output/grid.bin.json")
  testthat::expect_identical(output1$data, output2$data)

})

test_that("read_io warnings", {
  # NA values in subset
  testthat::expect_warning(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(cell = c(1, NA))
    ),
    "Removing NA values"
  )
  testthat::expect_warning(
    testthat::expect_warning(
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
  testthat::expect_warning(
    read_io(tmp_filename),
    "Header in file.*has invalid order"
  )
  file.remove(tmp_filename)

  # Attempt to overwrite name in clm file
  testthat::expect_warning(
    read_io("../testdata/output/pft_npp.clm", name = "LPJDUMMY"),
    "You cannot overwrite the header name in clm files"
  )

  # Duplicated year subset
  testthat::expect_warning(
    read_io(
      "../testdata/output/pft_npp.bin.json",
      subset = list(year = c("2001", "2001"))
    ),
    "Removing.+duplicate.+entr"
  )
  testthat::expect_warning(
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
  testthat::expect_message(
    testthat::expect_message(
      read_io(tmp_filename),
      "Detected.+nbands.*consider setting"
    ),
    "Adding default values for"
  )
  file.remove(tmp_filename)
})
