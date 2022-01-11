test_that("create job_details object", {

  model_path <- "TEST/PATH"

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )
  attr(test_params, "stages") <- c("config")

  test_submit <- submit_lpjml(test_params, model_path, no_submit = TRUE)

  test_params$job_id <- NA
  test_params$type <- "simulation"
  test_params$status <- "not submitted"

  expect_true(all(unlist(as.list(test_submit)) == unlist(as.list(test_params)),
              na.rm = TRUE))
})