# outputs in RAW format
test_that("read output - .bin - pft-specific & annual", {
  out_file_name <- "../testdata/output/pft_npp.bin"
  out <- read_output(file_name = out_file_name, file_type = "raw")
  expect_false(exists("out"))
})

# outputs in CLM format
test_that("read output - .clm - pft-specific & annual", {
  out_file_name <- "../testdata/output/pft_npp.clm"
  out <- read_output(file_name = out_file_name, file_type = "clm")
  expect_true(exists("out"))
  expect_type(out1, "LpjmlData")
})

# ------------------------------------ #
# outputs in META format & different time steps
test_that("read output - .bin.json - daily", {
  out_file_name <- "../testdata/output/d_lai.bin.json"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - .bin.json - monthly", {
  out_file_name <- "../testdata/output/transp.bin.json"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - .bin.json - annual", {
  out_file_name <- "../testdata/output/npp.bin.json"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - .bin.json - pft-specific & annual", {
  out_file_name <- "../testdata/output/pft_npp.bin.json"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out1, "LpjmlData")
})