# Test add_grid method
test_that("test add_grid method", {
  file_name <- "../testdata/output/npp.bin.json"
  output <- read_io(filename = file_name)
  output_manual <- add_grid(
    output,
    filename = "../testdata/output/grid.bin.json"
  )
  # Perform adding a grid object, which causes a message
  expect_message(
    output$add_grid(),
    "grid.bin.json"
  )
  # Read in grid directly and initialize object as LPJmLGridData
  grid <- read_io("../testdata/output/grid.bin.json") %>%
    LPJmLGridData$new()

  # Check if added grid equals grid file (read in separately)
  expect_equal(output$grid, grid)
  expect_equal(output_manual$grid, grid)

  # Check grid cells with those of objects meta data
  expect_equal(
    dimnames(output$grid$data)$cell,
    format(
      seq(output$meta$firstcell, length.out = output$meta$ncell),
      trim = TRUE, scientific = FALSE
    )
  )

  # Test spatial subset
  output <- read_io(filename = file_name, subset = list(cell = 1))

  # Perform adding a grid object
  output_manual <- add_grid(
    output,
    filename = "../testdata/output/grid.bin.json"
  )

  expect_message(
    output$add_grid(),
    "grid.bin.json"
  )

  # Read in grid directly, spatial subset and initialize object as LPJmLGridData
  grid <- read_io(
    "../testdata/output/grid.bin.json",
    subset = list(cell = 1)
  ) %>%
    LPJmLGridData$new()

  # Check if added grid equals grid file (read in separately)
  expect_equal(output$grid, grid)
  expect_equal(output_manual$grid, grid)

})

test_that("test LPJmLGridData methods", {

  # Read in grid directly, spatial subset and initialize object as LPJmLGridData
  grid <- read_io(
    "../testdata/output/grid.bin.json",
    variable = "LPJGRID"
  ) %>%
    suppressWarnings() %>%
    LPJmLGridData$new()

  # Check if meta data printed
  expect_output(
    print(grid),
    "meta"
  )
  expect_output(
    print(grid),
    "data"
  )

  # Check if data printed
  expect_output(
    print(grid),
    "band"
  )
  expect_output(
    print(grid),
    "lat"
  )

  expect_equal(
    grid$meta$variable,
    "grid"
  )

  # Check for methods that are not valid for LPJmLGridData

  expect_error(
    grid$add_grid(),
    "Not allowed for an object of class LPJmLGridData"
  )

  expect_error(
    grid$plot(),
    "No plot function available for LPJmLGridData object."
  )

  # Only variable "grid" (and "LPJGRID") are valid to init LPJmLGridData
  expect_error(
    read_io(
      "../testdata/output/npp.bin.json",
    ) %>%
      LPJmLGridData$new(),
    "Supported variables are"
  )
})
