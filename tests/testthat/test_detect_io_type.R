test_that("Detect valid I/O types", {
  expect_equal(
    detect_io_type("../testdata/output/pft_npp.bin.json"),
    "meta"
  )
  expect_equal(
    detect_io_type("../testdata/output/pft_npp.bin"),
    "raw"
  )
  expect_equal(
    detect_io_type("../testdata/output/pft_npp.clm"),
    "clm"
  )

  # Simple text file
  tmp_filename <- tempfile("lpjmlkit")
  writeLines("Hello World", tmp_filename)
  expect_equal(detect_io_type(tmp_filename), "text")
  file.remove(tmp_filename)

  # Error for non-existing file
  expect_error(
    detect_io_type(tmp_filename),
    "File.*does not exist"
  )
})
