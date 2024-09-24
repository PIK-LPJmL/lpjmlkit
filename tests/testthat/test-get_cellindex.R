test_that("get_cellindex handles invalid file path", {

  expect_error(
    get_cellindex(""),
    "grid_filename must be a string representing a valid file path."
  )

  expect_error(
    get_cellindex("../testdata/output/invalid_file_path"),
    "grid_filename does not exist."
  )
})

test_that("get_cellindex handles invalid coordinates", {
  expect_error(
    get_cellindex("../testdata/output/grid.bin.json",
      coordinates = list(c(1.25, 2.75), c(1.25, 2.75, 3.25))
    ),
    "The two vectors in coordinates must have the same length"
  )
})

test_that("get_cellindex handles both extent and coordinates provided", {
  expect_error(
    get_cellindex("../testdata/output/grid.bin.json",
      extent = c(1.25, 2.75, 3.25, 4.75),
      coordinates = list(c(1.25, 2.75), c(1.25, 2.75))
    ),
    "Multiple subset options provided. Please provide only"
  )
})

test_that("get_cellindex handles extent values out of order", {
  expect_warning(
    get_cellindex("../testdata/output/grid.bin.json",
      extent = c(-88.25, -87.25, 25.75, 55.25)
    ),
    "Extent values out of bounds:"
  )
})

test_that("get_cellindex handles valid coordinates", {
  expect_warning(
    get_cellindex(
      "../testdata/output/grid.bin.json",
      coordinates = list(lon = c("-87.25", -87.25), lat = c(55.25, 55.75))
    ),
    "Non-numeric coordinates detected"
  )
})

test_that("get_cellindex returns correct cell index for given extent", {
  result <- get_cellindex("../testdata/output/grid.bin.json",
    extent = c(-87.25, -87.25, 55.25, 55.75)
  )
  expect_true(length(result) == 2 && result[1] == 10001 && result[2] == 10002)
})

test_that("get_cellindex returns correct cell index for given coordinates", {
  result <- get_cellindex("../testdata/output/grid.bin.json",
    coordinates = list(lon = c(-87.25, -87.25), lat = c(55.25, 55.75))
  )
  expect_true(length(result) == 2 && result[1] == 10001 && result[2] == 10002)
})

test_that("get_cellindex returns correct cell index for a given shape", {
  result <- get_cellindex("../testdata/output/grid.bin.json",
                          shape = terra::vect(terra::ext(c(-87.25, -87.25,
                                                           55.25, 55.75)),
                                              crs = "+proj=longlat +datum=WGS84"))
  expect_true(length(result) == 2 && result[1] == 10002 && result[2] == 10001)
})
