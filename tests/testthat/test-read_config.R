
test_that("read in json data", {
  # check if json file is read in correctly
  json_data <- read_config("../testdata/test_config.json")
  testthat::expect_true(exists("json_data"))
  testthat::expect_true(methods::is(json_data, "list"))

  # Check for checking required elements for valid config
  testthat::expect_error(
    read_config("../testdata/lpjml.js"),
    "Missing element"
  )
})
