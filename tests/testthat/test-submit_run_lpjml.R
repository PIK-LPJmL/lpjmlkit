testthat::test_that("check submit_lpjml with tibble", {

  skip_on_os("windows")

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )
  attr(test_params, "stages") <- c("config")

  # Check submit_lpjml directly (and only)
  test_submit <- submit_lpjml(
    test_params,
    "../testdata",
    no_submit = TRUE
  )

  test_params$job_id <- NA
  test_params$type <- "simulation"
  test_params$status <- "not submitted"

  testthat::expect_true(all(unlist(as.list(test_submit)) == unlist(as.list(test_params)),
              na.rm = TRUE))

  # Check submit_run functionality
  test_submit <- submit_lpjml(
    test_params,
    "../testdata"
  )
  test_params$status <- "failed"

  testthat::expect_true(all(unlist(as.list(test_submit)) == unlist(as.list(test_params)),
              na.rm = TRUE))
})


testthat::test_that("check submit_lpjml with character string", {

  skip_on_os("windows")

  # Check submit_lpjml directly (and only)
  test_submit <- submit_lpjml(
    "./config_scen1_spinup.json",
    "../testdata",
    no_submit = TRUE
  )
  testthat::expect_true("scen1_spinup" %in% test_submit$sim_name)

  # Check submit_run functionality
  test_submit <- submit_lpjml(
    "./config_scen1_spinup.json",
    "../testdata"
  )
  testthat::expect_true("failed" %in% test_submit$status)

})


testthat::test_that("raise submit_lpjml errors", {

  skip_on_os("windows")

  # Check if directory is valid
  testthat::expect_error(
    submit_lpjml(
      "./config_scen1_spinup.json",
      "does/not/exist",
      no_submit = TRUE
    ),
    "Folder of model_path"
  )
})


testthat::test_that("raise run_lpjml errors", {

  skip_on_os("windows")

  # Check if directory is valid
  testthat::expect_error(
    run_lpjml(
      "./config_scen1_spinup.json",
      "does/not/exist",
    ),
    "Folder of model_path"
  )

  # Check if system command failes
  testthat::expect_error(
    run_lpjml(
      "./config_scen1_spinup.json"
    ),
    "System command"
  )

  # Check if working in a SLURM job environment
  testthat::expect_error(
    run_lpjml(
      "./config_scen1_spinup.json",
      parallel_cores = 4
    ),
    "Parallelization is only supported"
  )

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )

  # Complete run_lpjml without raising an error when running LPJmL fails
  test_run <- run_lpjml(
    test_params,
    raise_error = FALSE
  )
  testthat::expect_true("run" %in% test_run$status)

  # Check if working in a SLURM job environment (other conditions with tibble)
  testthat::expect_error(
    run_lpjml(
      test_params,
      parallel_cores = 4
    ),
    "Parallelization is only supported"
  )

})
