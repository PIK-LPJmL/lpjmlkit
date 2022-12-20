# outputs in RAW format
test_that("read io - .bin - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.bin"
  out <- read_io(filename = out_filename,
                 file_type = "raw",
                 nbands = 43,
                 firstcell = 10000,
                 ncell = 3,
                 firstyear = 2001,
                 nyear = 11)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

# outputs in CLM format
test_that("read io - .clm - pft-specific & annual", {
  out_filename <- "../testdata/output/pft_npp.clm"
  out <- read_io(filename = out_filename, file_type = "clm")
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

# ------------------------------------ #
# outputs in META format & different time steps
test_that("read io - .bin.json - daily", {
  out_filename <- "../testdata/output/d_lai.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
})

test_that("read io - .bin.json - monthly", {
  out_filename <- "../testdata/output/transp.bin.json"
  out <- read_io(filename = out_filename)
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
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
