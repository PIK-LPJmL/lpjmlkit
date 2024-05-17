test_that("test find_varfile", {

  # grid file in directory matching search pattern
  expect_match(
    find_varfile("../testdata/output", "grid"),
    "testdata/output/grid.bin.json"
  )

  # Error due to missing grid file
  expect_error(
    find_varfile(".", "grid"),
    "Cannot detect grid file automatically"
  )

})

