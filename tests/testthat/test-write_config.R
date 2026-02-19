
test_that("write correct config with dot syntax", {

  skip_on_os("mac")

  test_params <- tibble::tibble(
    sim_name = "spinup_pnv",
    random_seed = as.integer(42),
    landuse = "no",
    wateruse = NA,
    param.k_temp = 0.03,
    soilpar.1.name = "clark",
    fbd_fac = list(c(1.0, 1.2, 1.42, 0.0)),
    fbd_int = list(c(1, 1, 1, 42)),
    order = 1,
    dependency = NA
  )

  # Create template that would usually be created by write_config directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # Test main function of write_config since write_config is hard to test
  tmp_objects <- write_single_config(
    x = test_params,
    model_path = "../testdata",
    sim_path = "../testdata",
    output_list = c(),
    output_list_timestep = "annual",
    output_format = "clm",
    cjson_filename = "lpjml_config.cjson",
    config_tmp = test_tmp,
    slurm_args = slurm_args
  )

  # Check json mutate functions to result in correct json file
  check_json <- read_config("../testdata/config_spinup_pnv.json")
  expect_true(all(unlist(tmp_objects[[1]]) %in% unlist(check_json)))


  # Check returned tibble to come in the right format with equal data
  check_tibble <- test_tmp
  check_tibble[1,
    c("sim_name", "dependency", "order")
  ] <- list("spinup_pnv", NA, 1)
  expect_true(
    all(
      tmp_objects[[2]][1, which(tmp_objects != "dependency")] %in%
        check_tibble[1, which(tmp_objects != "dependency")]
    )
  )

  # Check for non valid config/param
  test_params["test_param"] <- TRUE
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "consists of key"
  )
  test_params["test_param"] <- NULL

  # Check for non valid config/param combination
  test_params["sim_name.landuse"] <- TRUE
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "include a combination of keys or indices that do not exist"
  )
})


test_that("write correct config with common list syntax", {

  skip_on_os("mac")

  test_params <- tibble::tibble(
    sim_name = "spinup_pnv",
    random_seed = as.integer(42),
    landuse = "no",
    wateruse = NA,
    `param$k_temp` = 0.03,
    `soilpar[[1]][["name"]]` = "clark",
    fbd_fac = list(c(1.0, 1.2, 1.42, 0.0)),
    dependency = NA
  )

  # Create template that would usually be created by write_config directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # Test main function of write_config since write_config is hard to test
  tmp_objects <- write_single_config(
    x = test_params,
    model_path = "../testdata",
    sim_path = "../testdata",
    output_list = c(),
    output_list_timestep = "annual",
    output_format = "clm",
    cjson_filename = "lpjml_config.cjson",
    config_tmp = test_tmp,
    slurm_args = slurm_args
  )

  # Check json mutate functions to result in correct json file
  check_json <- read_config("../testdata/config_spinup_pnv.json")
  expect_true(all(unlist(tmp_objects[[1]]) %in% unlist(check_json)))

  # Check for non valid config/param
  test_params["test_param$check"] <- TRUE
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "consists of keys that do not exist"
  )
  test_params["test_param$check"] <- NULL

  # Check for non valid config/param combination
  test_params["sim_name$landuse"] <- TRUE
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "include a combination of keys or indices that do not exist"
  )
  test_params["sim_name$landuse"] <- NULL

  # A sim_name is missing (works as identifier)
  test_params["sim_name"] <- NULL
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "A sim_name is missing in"
  )
  test_params["sim_name"] <- "spinup_pnv"


  # Missing dependency (if order specified)
  test_params["order"] <- 2
  test_params["dependency"] <- NULL
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "field dependency is missing"
  )

  # Non valid order (< 1)
  test_params["order"] <- -1
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "order is not valid!"
  )

  # Combination of config/param that requires a restart_filename (not specified)
  test_params["order"] <- 1
  test_params["restart_filename"] <- NULL
  expect_warning(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c(),
      output_list_timestep = "annual",
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "please make sure to explicitly set restart_filename"
  )
  test_params["restart_filename"] <- NULL


  # Non valid output timestep (subannual)
  test_params["dependency"] <- "spinup_spinup"
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c("irrig", "irrig"),
      output_list_timestep = c("subannual", "subannual"),
      output_format = "clm",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "No valid output_timestep"
  )


  # Non matching length if output_list and output_list_timestep
  expect_error(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c("irrig"),
      output_list_timestep = c("annual", "annual"),
      output_format = "cdf",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "output_timestep does not have a valid length"
  )

  # Non valid output
  expect_warning(
    write_single_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = c("soil_health"),
      output_list_timestep = c("annual"),
      output_format = "cdf",
      cjson_filename = "lpjml_config.cjson",
      config_tmp = test_tmp,
      slurm_args = slurm_args
    ),
    "not available in current model version"
  )

  # Check setting a macro
  test_params["wtime"] <- "10:00:00"
  test_params["-DMY_MACRO"] <- TRUE
  tmp_objects <- write_single_config(
    x = test_params,
    model_path = "../testdata",
    sim_path = "../testdata",
    output_list = c(),
    output_list_timestep = "annual",
    output_format = "clm",
    cjson_filename = "lpjml_config.cjson",
    config_tmp = test_tmp,
    slurm_args = slurm_args
  )
  expect_true(!"-DMY_MACRO" %in% tmp_objects)
})


test_that("include non output defined outputvars", {

  skip_on_os("mac")

  test_params <- tibble::tibble(
    sim_name = c("transient_pnv"),
    order = c(2),
    dependency = c("testdep"),
  )

  # create template that would usually be created by write_config directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA

  # test main function of write_config since write_config is hard to test
  tmp_objects <- write_single_config(
    x = test_params,
    model_path = "../testdata",
    sim_path = "../testdata",
    output_list = c("grid", "irrig"),
    output_list_timestep = "annual",
    output_format = "clm",
    cjson_filename = "lpjml_config.cjson",
    config_tmp = test_tmp,
    slurm_args = slurm_args
  )

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


# Test get_order function (for dependency)
test_that("get order", {

  test_params <- data.frame(
    sim_name = c("spinup", "landuse", "future_landuse"),
    dependency = c(NA, "spinup", "landuse")
  )

  order_params <- get_order(test_params)
  expect_equal(order_params$order, c(1, 2, 3))
})


# Test output_list as named list
test_that("write config with output_list named list", {
  
  test_params <- tibble::tibble(
    sim_name = "test_output_config",
    random_seed = as.integer(42)
  )
  
  # Create output_list as named list with various attributes
  # Note: Only using outputs that exist in test config (grid, irrig)
  test_output_list <- list(
    grid = list(),
    irrig = list(timestep = "monthly", format = "raw", scale = 0.5, offset = 5.0, filename = "irrig_custom")
  )
  
  # Create template that would usually be created by write_config directly
  test_tmp <- tibble::tibble(sim_name = NA,
                             order = NA,
                             dependency = NA)
  slurm_args <- c("sclass", "ntask", "wtime", "blocking")
  test_tmp[slurm_args] <- NA
  
  # Test write_single_config with output_list as named list
  tmp_objects <- write_single_config(
    x = test_params,
    model_path = "../testdata",
    sim_path = "../testdata",
    output_list = names(test_output_list),
    output_list_timestep = "annual",
    output_format = "clm",
    output_config = test_output_list,
    cjson_filename = "lpjml_config.cjson",
    config_tmp = test_tmp,
    slurm_args = slurm_args
  )
  
  # Check that outputs were created
  expect_true(length(tmp_objects[[1]][["output"]]) == 2)
  
  # Check grid output (should not have timestep since it's grid)
  grid_output <- tmp_objects[[1]][["output"]][[1]]
  expect_equal(grid_output$id, "grid")
  
  # Check irrig output has correct attributes from output_list named list
  irrig_output <- tmp_objects[[1]][["output"]][[2]]
  expect_equal(irrig_output$id, "irrig")
  expect_equal(irrig_output$file$fmt, "raw")
  expect_equal(irrig_output$file$timestep, "monthly")
  expect_equal(irrig_output$file$scale, 0.5)
  expect_equal(irrig_output$file$offset, 5.0)
  expect_match(irrig_output$file$name, "irrig_custom\\.bin$")
})


# Test output_list named list validation
test_that("output_list named list validation", {
  
  test_params <- tibble::tibble(
    sim_name = "test",
    random_seed = as.integer(42)
  )
  
  # Test missing names
  bad_list1 <- list(list(timestep = "annual"))
  expect_error(
    write_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = bad_list1,
      cjson_filename = "lpjml_config.cjson"
    ),
    "must be named with output IDs"
  )
  
  # Test invalid timestep
  bad_list2 <- list(irrig = list(timestep = "subannual"))
  expect_error(
    write_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = bad_list2,
      cjson_filename = "lpjml_config.cjson"
    ),
    "Invalid timestep value"
  )
  
  # Test invalid format
  bad_list3 <- list(irrig = list(format = "txt"))
  expect_error(
    write_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = bad_list3,
      cjson_filename = "lpjml_config.cjson"
    ),
    "Invalid format value"
  )

  # Tibble/data.frame should be rejected
  bad_df <- data.frame(id = "irrig", timestep = "annual")
  expect_error(
    write_config(
      x = test_params,
      model_path = "../testdata",
      sim_path = "../testdata",
      output_list = bad_df,
      cjson_filename = "lpjml_config.cjson"
    ),
    "Each element of output_list must be a list"
  )
})

