
test_that("read in json data", {
  # check if json file is read in correctly
  json_data <- read_config("../testdata/lpjml.js")
  expect_true(exists("json_data"))
  expect_true(methods::is(json_data, "list"))

  # Check for checking required elements for valid config
  expect_error(
    read_config("../testdata/lpjml_nonvalid.js"),
    "Missing element"
  )
})
