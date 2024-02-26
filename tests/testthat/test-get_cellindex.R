test_that("get_cellindex handles invalid file path", {
  expect_error(get_cellindex("../testdata/output/invalid_file_path"))
})

test_that("get_cellindex handles invalid extent", {
  expect_error(get_cellindex("../testdata/output/grid.bin.json",
    extent = c("1.25", "2.75", "3.25", "4.75")
  ))
})

test_that("get_cellindex handles invalid coordinates", {
  expect_error(get_cellindex("../testdata/output/grid.bin.json",
    coordinates = list(c(1.25, 2.75), c(1.25, 2.75, 3.25))
  ))
  expect_error(get_cellindex("../testdata/output/grid.bin.json",
    coordinates = list(c("1.25", "2.75"), c("1.25", "2.75"))
  ))
})

test_that("get_cellindex handles both extent and coordinates provided", {
  expect_error(get_cellindex("../testdata/output/grid.bin.json",
    extent = c(1.25, 2.75, 3.25, 4.75),
    coordinates = list(c(1.25, 2.75), c(1.25, 2.75))
  ))
})

test_that("get_cellindex handles extent values out of order", {
  expect_warning(get_cellindex("../testdata/output/grid.bin.json",
    extent = c(-88.25, -87.25, 25.75, 55.25)
  ))
})

test_that("get_cellindex handles valid extent", {
  expect_silent(get_cellindex("../testdata/output/grid.bin.json",
    extent = c(-87.25, -87.25, 55.25, 55.75)
  ))
})

test_that("get_cellindex handles valid coordinates", {
  expect_silent(get_cellindex("../testdata/output/grid.bin.json",
    coordinates = list(c(-87.25, -87.25), c(55.25, 55.75))
  ))
})

test_that("get_cellindex returns correct cell index for given extent", {
  result <- get_cellindex("../testdata/output/grid.bin.json",
    extent = c(-87.25, -87.25, 55.25, 55.75)
  )
  expect_true(length(result) == 2 && result[1] == 10001 && result[2] == 10002)
})

test_that("get_cellindex returns correct cell index for given coordinates", {
  result <- get_cellindex("../testdata/output/grid.bin.json",
    coordinates = list(c(-87.25, -87.25), c(55.25, 55.75))
  )
  expect_true(length(result) == 2 && result[1] == 10001 && result[2] == 10002)
})
