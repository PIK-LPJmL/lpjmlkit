
test_that("write correct config with dot syntax", {

    test_params <- data.frame(
      sim_name = "spinup_pnv",
      random_seed = as.integer(42),
      landuse = "no",
      param.k_temp = 0.03,
      soilpar.1.name = "clark",
      order = 1,
      dependency = NA
      )

  # create template that would usually be created by writeConfig directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # test main function of writeConfig since writeConfig is hard to test
  tmp_objects <- write_single_config(params = test_params,
                                     model_path = "testthat",
                                     output_path = "testthat",
                                     output_list = c(),
                                     output_list_timestep = "annual",
                                     output_format = "clm",
                                     js_filename = "lpjml.js",
                                     config_tmp = test_tmp,
                                     slurm_args = slurm_args,
                                     test_it = TRUE)

  # check json mutate functions to result in correct json file
  check_json <- read_config("../testdata/config_spinup_pnv.json")
  expect_true(all(unlist(tmp_objects[[1]]) %in% unlist(check_json)))


  # check returned tibble to come in the right format with equal data
  check_tibble <- test_tmp
  check_tibble[1,
    c("sim_name", "dependency", "order")
  ] <- list("spinup_pnv", NA, 1)
  expect_true(all(
      tmp_objects[[2]][1, which(tmp_objects != "dependency")] %in%
      check_tibble[1, which(tmp_objects != "dependency")]))

})


test_that("write correct config with common list syntax", {

    test_params <- tibble::tibble(
      sim_name = "spinup_pnv",
      random_seed = as.integer(42),
      landuse = "no",
      `param$k_temp` = 0.03,
      `soilpar[[1]][["name"]]` = "clark",
      dependency = NA
      )

  # create template that would usually be created by writeConfig directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # test main function of writeConfig since writeConfig is hard to test
  tmp_objects <- write_single_config(params = test_params,
                                     model_path = "testthat",
                                     output_path = "testthat",
                                     output_list = c(),
                                     output_list_timestep = "annual",
                                     output_format = "clm",
                                     js_filename = "lpjml.js",
                                     config_tmp = test_tmp,
                                     slurm_args = slurm_args,
                                     test_it = TRUE)

  # check json mutate functions to result in correct json file
  check_json <- read_config("../testdata/config_spinup_pnv.json")
  expect_true(all(unlist(tmp_objects[[1]]) %in% unlist(check_json)))

})



test_that("include non output defined outputvars", {

    test_params <- tibble::tibble(
      sim_name = c("transient_pnv"),
      order = c(2),
      dependency = c("testdep"),
      )

  # create template that would usually be created by writeConfig directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # test main function of writeConfig since writeConfig is hard to test
  tmp_objects <- write_single_config(params = test_params,
                                     model_path = "testthat",
                                     output_path = "testthat",
                                     output_list = c("grid", "irrig"),
                                     output_list_timestep = "annual",
                                     output_format = "clm",
                                     js_filename = "lpjml.js",
                                     config_tmp = test_tmp,
                                     slurm_args = slurm_args,
                                     test_it = TRUE)

  # check if defined outputvar (id) exists as last output
  expect_true(
    tmp_objects[[1]][["output"]][[
      length(tmp_objects[[1]][["output"]])
    ]]$id == "irrig"
  )

  # check if filename is set correctly
  expect_true(
    grepl("irrig.clm",
          tmp_objects[[1]][["output"]][[
            length(tmp_objects[[1]][["output"]])
          ]]$file$name)
  )

})
