# outputs in raw format with meta file
test_that("read output - daily", {
  # test daily output
  out_file_name <- "../testdata/output/d_lai.bin"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - monthly", {
  # test monthy output
  out_file_name <- "../testdata/output/transp.bin"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - annual", {
  # test annual output
  out_file_name <- "../testdata/output/npp.bin"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - pft-specific & annual", {
  # test pft-annual output
  out_file_name <- "../testdata/output/pft_npp.bin"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out1, "LpjmlData")
})

# ------------------------------------ #
# outputs in clm format
test_that("read output - daily", {
  # test daily output
  out_file_name <- "../testdata/output/d_lai.clm"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - monthly", {
  # test monthy output
  out_file_name <- "../testdata/output/transp.clm"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - annual", {
  # test annual output
  out_file_name <- "../testdata/output/npp.clm"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out, "LpjmlData")
})

test_that("read output - pft-specific & annual", {
  # test pft-annual output
  out_file_name <- "../testdata/output/pft_npp.clm"
  out <- read_output(file_name = out_file_name)
  expect_true(exists("out"))
  expect_type(out1, "LpjmlData")
})