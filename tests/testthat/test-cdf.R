# outputs in cdf format
test_that("read cdf - read, attach grid, transform to cell", {

  grid_file <- "../testdata/output/grid.bin.json"
  grd <- read_io(grid_file)

  # test reading the file
  out_filename <- "../testdata/output/npp.nc"
  w <- capture_warnings(out <- read_io(filename = out_filename))
  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
  expect_match(w, ".*Only one longitude value found.*", all = FALSE)

  # test attaching a grid and converting to LPJmL array
  out_transformed <- out |>
    lpjmlkit::transform(to = c("year_month_day")) |>
    lpjmlkit::add_grid(grid_file) |>
    lpjmlkit::transform(to = "cell") |>
    lpjmlkit::as_array() |>
    drop()

  expect_true(exists("out_transformed"))

})

test_that("read cdf - test reading different grid files", {
  griddata2 <- read_io("../testdata/output/grid.nc.json")
  griddata3 <- read_io("../testdata/output/grid.nc")

  expect_identical(
    c(griddata2$meta$variable,
      griddata2$meta$format
    ),
    c(griddata3$meta$variable,
      griddata2$meta$format
    )
  )
})

test_that("read cdf - read 2 dim and 3 dim inputs and manual parameter setting", {

  grid_file <- "../testdata/output/grid.bin.json"
  grid <- read_io(filename = grid_file)
  expect_true(exists("grid"))

  # and 2 banded?
  mask_file <- "../testdata/input/mask_ice_2cells.nc"
  w <- capture_warnings(mask <- read_io(filename = mask_file))
  w2 <- capture_warnings(mask2 <- read_io(filename = mask_file, nstep = 1))

  expect_match(w, ".*None of the variables could certainly be identified as time variable.*", all = FALSE)
  expect_match(w, ".*Time information could not be extracted.*", all = FALSE)
  expect_match(w2, ".*None of the variables could certainly be identified as time variable.*", all = FALSE)
  expect_match(w2, ".*Time information could not be extracted.*", all = FALSE)
  expect_true(exists("mask"))
  expect_identical(
    c(mask$meta$nstep,
      mask2$meta$nstep
    ),
    c(NULL,
      1
    )
  )
})


# outputs in cdf format
test_that("check ncell when attaching grid and transforming to cell", {
  # test reading the file
  out_filename <- "../testdata/output/mnpp.nc"
  out <-  read_io(filename = out_filename)

  expect_true(exists("out"))
  expect_true("LPJmLData" %in% class(out))
  expect_true(is.na(out$meta$ncell))

  # now attach a grid and converting to LPJmL array
  grid_file <- "../testdata/output/grid.nc"

  w <- capture_warnings(
    out_transformed <- out |>
      lpjmlkit::add_grid(grid_file) |>
      lpjmlkit::transform(to = "cell")
  )
  expect_true(exists("out_transformed"))
  expect_identical(
    out_transformed$meta$ncell,
    as.integer(3)
  )

  out_back_transformed <- out_transformed |>
    lpjmlkit::transform(to = "lon_lat")

  expect_identical(
    out_back_transformed$meta$ncell,
    as.integer(3) # this should be changed
  )
})


test_that("add test case comparing reading .nc.json and .nc", {
  # reading
  nc_filename <- "../testdata/output/mnpp.nc"
  nc <- read_io(filename = nc_filename)
  json_filename <- "../testdata/output/mnpp.nc.json"
  json <- read_io(filename = json_filename)

  expect_identical(
    c(nc$dimnames()$time[1],
      nc$meta$band_names,
      as.integer(nc$meta$nbands)
    ),
    c(json$dimnames()$time[1],
      json$meta$band_names,
      as.integer(json$meta$nbands)
    )
  )
})


test_that("read cdf - check requesting a secondary variable", {
  # reading
  nc_filename <- "../testdata/input/ERA5_4cells_1year12months.nc"

  w <- capture_warnings(nc <- read_io(filename = nc_filename, variable = "stl1"))

  expect_match(w, ".*Non-standard time dimension in netcdf file identified as: .*", all = FALSE)
  expect_identical(
    c(round(nc$data[1, 2, 12, ], 3),
      nc$dimnames()$time[1]
    ),
    c(274.623,
      "1980-01-31"
    )
  )
})

test_that("read cdf - test deficient cdf implementation", {

  out_filename <- "../testdata/output/pft_npp.nc"
  file_nc <- ncdf4::nc_open(filename = out_filename)

  # test if unit is only specified as year
  file_nc$dim$time$units <- "year"
  time_cf <- lpjmlkit:::get_timestep(file_nc)
  expect_true(exists("time_cf"))

  # test empty time unit
  file_nc$dim$time$units <- NULL
  time_cf <- get_timestep(file_nc)
  expect_identical(
    time_cf, NULL
  )


})

test_that("read cdf - test lat/lon_bnds", {

  out_filename <- "../testdata/input/grid_2cell.nc4"
  grid <- read_cdf_meta(filename = out_filename, variable_name = "cellid")
  expect_true(exists("grid"))

})

test_that("read cdf - isimip subsetted monthly banded", {

  out_filename <- "../testdata/output/soilmoist_band_2cell_1year_12months.nc"
  meta <- read_meta(filename = out_filename)

  expect_true(exists("meta"))
  expect_identical(
    c(meta$nyear,
      meta$variable,
      meta$nbands,
      meta$ncell,
      meta$datatype,
      meta$band_names[5]),
    c(1,
      "rootmoist",
      5,
      NA,
      "float",
      "2.5"
    )
  )
})

test_that("read cdf - banded input + subsetting", {

  grid_file <- "../testdata/output/grid.bin.json"

  out_filename <- "../testdata/output/pft_npp.nc"
  w <- capture_warnings(
    out <- read_io(filename = out_filename,
                   subset = list(year = as.character(2002:2003),
                                 band = c("Polar C3 grass", "rainfed pulses"))) |>
      lpjmlkit::transform(to = c("year_month_day")) |>
      lpjmlkit::add_grid(grid_file) |>
      lpjmlkit::transform(to = "cell") |>
      lpjmlkit::as_array() |>
      drop()
  )
  expect_match(w, ".*Only one longitude value found.*", all = FALSE)
  expect_match(w, ".*Automatic band dimensions detection.*", all = FALSE)
  expect_match(w, ".*Automatic band name dimensions detection.*", all = FALSE)

  expect_true(exists("out"))

})

test_that("read cdf - banded input", {

  grid_file <- "../testdata/output/grid.bin.json"

  # test a monthly output
  out_filename <- "../testdata/output/transp.nc"

  w <- capture_warnings(
    out <- read_io(filename = out_filename) |>
      lpjmlkit::transform(to = c("year_month_day")) |>
      lpjmlkit::add_grid(grid_file) |>
      lpjmlkit::transform(to = "cell") |>
      lpjmlkit::as_array() |>
      drop()
  )
  expect_match(w, ".*Only one longitude value found.*", all = FALSE)
  expect_true(exists("out"))

})

test_that("read cdf - daily input", {

  grid_file <- "../testdata/output/grid.bin.json"

  # test daily input
  out_filename <- "../testdata/output/ddischarge_1901-1902.nc"
  w <- capture_warnings(
    out <- read_io(out_filename) |>
      lpjmlkit::transform(to = c("year_month_day")) |>
      lpjmlkit::add_grid(grid_file) |>
      lpjmlkit::transform(to = "cell") |>
      lpjmlkit::as_array() |>
      drop()
  )
  expect_match(w, ".*Only one longitude value found.*", all = FALSE)
  expect_true(exists("out"))
  dims <- as.integer(c(3, 31, 12, 2))
  names(dims) <- c("cell", "day", "month", "year")
  expect_identical(
    dim(out),
    dims
  )
})

test_that("read cdf - read meta", {

  out_filename <- "../testdata/output/pft_npp.nc"
  meta <- read_meta(filename = out_filename)

  expect_true(exists("meta"))
  expect_identical(
    c(meta$nyear,
      meta$variable,
      meta$nbands,
      meta$ncell,
      meta$datatype,
      meta$band_names[16]),
    c(11,
      "pft_npp",
      43,
      NA,
      "float",
      "rainfed pulses"
    )
  )

})

test_that("read cdf - test leapday removal and cropping of non-lpjml cells", {

  out_filename <- "../testdata/input/ERA_sst_4cells_2000_daily_w_leapdays.nc"
  meta <- read_meta(filename = out_filename)

  expect_true(exists("meta"))
  expect_identical(
    meta$nstep, 365
  )
  w <- capture_warnings(
    data <- read_io(filename = out_filename)
  )

  # check if during adding grid, ocean data is being lost
  w <- capture_warnings(
    data <- read_io(filename = out_filename)
  )
  grid_file <- "../testdata/output/grid.nc"
  w <- capture_warnings(
    grd <- read_io(filename = grid_file)
  )
  w <- capture_warnings(
    data_grid_added <- data |>
      add_grid(grid_file)
  )

  w <- capture_warnings(
    data_transformed <- data_grid_added |>
      lpjmlkit::transform(to = "cell")
  )

  expect_identical(
    c(round(data$data[1, 1, 1, 1]),
      round(data_grid_added$data[1, 1, 1, 1]),
      round(data_transformed$data[1, 1, 1])
    ),
    c(287, # data
      287, # data still intact after adding grid
      NA # now data gets cropped to (different) grid extent -> NA
    )
  )

})

test_that("read cdf - test different dim_order", {

  out_filename <- "../testdata/output/npp.nc"
  w <- capture_warnings(
    npp_lat <- read_io(filename = out_filename,
      dim_order = c("lat", "lon", "time", "band")
    )
  )
  w2 <- capture_warnings(
    npp_lon <- read_io(filename = out_filename,
      dim_order = c("lon", "lat", "time", "band")
    )
  )
  expect_match(w, ".*Only one longitude value found.*", all = FALSE)
  expect_match(w2, ".*Only one longitude value found.*", all = FALSE)

  expect_identical(
    c(names(npp_lat$dimnames())[1],
      names(npp_lon$dimnames())[1]
    ),
    c("lat",
      "lon"
    )
  )

})
