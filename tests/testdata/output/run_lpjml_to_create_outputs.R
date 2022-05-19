# ---------------------------------------------------------------------------- #
# Project:     lpjmlKit
# ------------------------------------ #
# Author:      Sara Minoli
# Email:       sara.minoli@pik-potsdam.de
# Description: Run LPJmL to create a sample output file for package tests
# ---------------------------------------------------------------------------- #

rm(list = ls(all = T))

library(lpjmlKit)
library(tibble)
#library(magrittr)


# ------------------------------------ #
# Paths ----
lpjdir <- paste0("/home/minoli/LPJmL_GitLab/nitrogen_cotton_tillage_newoutput/")
outdir <- paste0("/home/minoli/git_lpjmlKit/lpjml_outputs/")
testdatadir <- paste0("/home/minoli/git_lpjmlKit/lpjmlKit/tests/testdata/")


# ------------------------------------ #
# Create Parameter Tibble ----
test_param <- tibble(
    sim_name         = c("test_spinup", "test_transient"),
    dependency       = c(NA, "test_spinup"),
    order            = c(1, 2),
    startgrid        = 10000,
    endgrid          = 10002,
    nspinup          = c(1000, 390),
    river_routing    = FALSE,
    `-DDAILY_OUTPUT` = c(FALSE, TRUE),
    firstyear           = 1901,
    lastyear            = 2011,
    outputyear          = 2001,
    output_metafile  = TRUE
)

outputs_df <- data.frame(rbind(
                         cbind(output = "d_lai",   timestep = "daily"),
                         cbind(output = "transp",  timestep = "monthly"),
                         cbind(output = "npp",     timestep = "annual"),
                         cbind(output = "pft_npp", timestep = "annual"),
                         cbind(output = "grid",    timestep = "annual")
                         ))

# ------------------------------------ #
# Write Config Files ----
config_details1 <- write_config(
  params               = test_param,
  model_path           = lpjdir,
  js_filename          = "lpjml.js",
  output_path          = outdir,
  output_format        = "raw",
  output_list          = outputs_df$output,
  output_list_timestep = outputs_df$timestep,
  debug                = TRUE
  )

config_details2 <- write_config(
  params               = test_param,
  model_path           = lpjdir,
  js_filename          = "lpjml.js",
  output_path          = outdir,
  output_format        = "clm",
  output_list          = outputs_df$output,
  output_list_timestep = outputs_df$timestep,
  debug                = TRUE
  )

# ------------------------------------ #
# Run the model
run_details <- run_lpjml(
  x           = config_details1,
  model_path  = lpjdir,
  output_path = outdir
)

run_details <- run_lpjml(
  x           = config_details2,
  model_path  = lpjdir,
  output_path = outdir
)

# ------------------------------------ #
# Copy Files to lpjmlKit test
outputs <- c(
  paste0(outputs_df$output, ".bin.json"),
  paste0(outputs_df$output, ".bin"),
  paste0("pft_npp", ".clm")
  )

if (all(file.exists(paste0(outdir, "output/test_transient/", outputs)))) {
    cat("\nOK! All outputs exist.")
} else {
    stop("\nSome outputs are missing!")
}

file.copy(from = paste0(outdir,      "output/test_transient/", outputs),
          to   = paste0(testdatadir, "output/", outputs),
          overwrite = TRUE,
          copy.mode = TRUE)

cat("\n--- Done! ---")
