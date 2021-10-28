
test_that("read in json data", {
  # check if json file is read in correctly
  json_data <- read_config("../testdata/test_config.json")
  expect_true(exists("json_data"))
  expect_true(is(json_data, "list"))
})