
test_that("write correct config", {

    test_params <- tibble::tibble(
      sim_name = c("spinup_pnv"),
      random_seed = as.integer(c(42)),
      landuse = c("no"),
      param.k_temp = c(0.03),
      soilpar.1.name = c("clark"),
      order = c(1),
      dependency = c(NA),
      )

  # create template that would usually be created by writeConfig directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             config_file = NA,
                             order = NA,
                             dependency = NA)

  # test main function of writeConfig since writeConfig is hard to test
  tmp_objects <- write_single_config(params = test_params,
                                     model_path = "testthat",
                                     output_path = "testthat",
                                     output_list = c(),
                                     output_format = "clm",
                                     js_filename = "lpjml.js",
                                     config_tmp = test_tmp,
                                     test_it = TRUE)

  # check json mutate functions to result in correct json file
  check_json <- read_config("../testdata/config_spinup_pnv.json")
  expect_true(all(unlist(tmp_objects[[1]]) == unlist(check_json)))


  # check returned tibble to come in the right format with equal data
  check_tibble <- test_tmp
  check_tibble[1,
    c("sim_name", "config_file", "dependency", "order")
  ] <- list("spinup_pnv", "config_spinup_pnv.json", NA, 1)
  expect_true(all(
      tmp_objects[[2]][1, which(tmp_objects != "dependency")] ==
      check_tibble[1, which(tmp_objects != "dependency")]))

})


test_that("include non output defined outputvars", {

    test_params <- tibble::tibble(
      sim_name = c("transient_pnv"),
      order = c(2),
      dependency = c("testdep"),
      )

  # create template that would usually be created by writeConfig directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             config_file = NA,
                             order = NA,
                             dependency = NA)

  # test main function of writeConfig since writeConfig is hard to test
  tmp_objects <- write_single_config(params = test_params,
                                     model_path = "testthat",
                                     output_path = "testthat",
                                     output_list = c("grid", "irrig"),
                                     output_format = "clm",
                                     js_filename = "lpjml.js",
                                     config_tmp = test_tmp,
                                     test_it = TRUE)
  print(tmp_objects)
  # check if defined outputvar (id) exists as last output
  expect_true(
    tmp_objects[[1]][["output"]][[
      length(tmp_objects[[1]][["output"]])
    ]]$id == "irrig"
  )

  # check if filename is set correctly
  expect_true(
    grepl("mirrig.clm",
          tmp_objects[[1]][["output"]][[
            length(tmp_objects[[1]][["output"]])
          ]]$file$name)
  )

})