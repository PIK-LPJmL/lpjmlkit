test_that("create x object - job_details tibble", {

  model_path <- "TEST/PATH"

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )
  attr(test_params, "stages") <- c("config")

  test_lpjml_submit <- submit_lpjml(test_params, model_path, no_submit = TRUE)
  test_params$job_id <- NA
  test_params$type <- "simulation"
  test_params$status <- "not submitted"

  test_postprocess_submit <- submit_postprocess(test_lpjml_submit,
                                                select_sim = "scen1_transient",
                                                output_path = model_path,
                                                fun = print,
                                                args = list(),
                                                no_submit = TRUE)

  test_params$fun_name <- NA
  test_params <- tibble::add_row(test_params,
                                 sim_name = "scen1_transient",
                                 fun_name = "print",
                                 type = "postprocess",
                                 order = 3,
                                 dependency = "scen1_transient (2)",
                                 job_id = NA,
                                 status = "not submitted")

  expect_true(all(unlist(as.list(test_postprocess_submit)) ==
                  unlist(as.list(test_params)),
              na.rm = TRUE))
})

test_that("create x object - character vector", {

  model_path <- "TEST/PATH"

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )
  attr(test_params, "stages") <- c("config")

  test_lpjml_submit <- submit_lpjml(test_params, model_path, no_submit = TRUE)
  test_params$job_id <- NA
  test_params$type <- "simulation"
  test_params$status <- "not submitted"

  test_postprocess_submit <- submit_postprocess("scen1_transient",
                                                output_path = model_path,
                                                fun = print,
                                                args = list(),
                                                no_submit = TRUE)

  expect_true(test_postprocess_submit == "scen1_transient")
})

test_that("create x object - only output path", {

  model_path <- "TEST/PATH"

  test_params <- tibble::tibble(
    sim_name = c("scen1_spinup", "scen1_transient"),
    order = c(1, 2),
    dependency = c(NA, "scen1_spinup"),
  )
  attr(test_params, "stages") <- c("config")

  test_lpjml_submit <- submit_lpjml(test_params, model_path, no_submit = TRUE)
  test_params$job_id <- NA
  test_params$type <- "simulation"
  test_params$status <- "not submitted"

  test_postprocess_submit <- submit_postprocess(output_path = model_path,
                                                fun = print,
                                                args = list(),
                                                no_submit = TRUE)

  expect_true(is.null(test_postprocess_submit))
})