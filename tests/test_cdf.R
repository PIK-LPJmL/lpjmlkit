devtools::load_all("/p/projects/open/Fabian/LPJbox/lpjmlkit_read_cdf/")
#devtools::load_all("/home/stenzel/lpjmlkit/")

# annual
fpc_file <- "/p/projects/open/Fabian/runs/metrics_cdf/output/lu_1500_2016/fpc.nc"

meta <- lpjmlkit::read_cdf_header(nc_in_file = fpc_file)
data <- lpjmlkit::read_cdf(nc_in_file = fpc_file, nc_header = meta)
data_2000_2005 <- lpjmlkit::read_cdf(nc_in_file = fpc_file, nc_header = meta, 
                              subset = list(year = as.character(2000:2005), 
                                            band = c("natural stand fraction", "tropical broadleaved evergreen tree")))

# monthly
mnpp_file <- "/p/projects/open/Fabian/runs/metrics_cdf/output/lu_1500_2016/mnpp.nc"
monthly_meta <- lpjmlkit::read_cdf_header(nc_in_file = mnpp_file)
monthly_data <- lpjmlkit::read_cdf(nc_in_file = mnpp_file, 
                                   nc_header = monthly_meta)
monthly_data_2000_2005 <- lpjmlkit::read_cdf(nc_in_file = mnpp_file, 
                                   nc_header = monthly_meta,
                                subset = list(year = as.character(2000:2005)))

# daily
daily_discharge <- "/p/projects/isimip/isimip/ISIMIP2b/OutputData/water_global/LPJmL/hadgem2-es/historical/lpjml_hadgem2-es_ewembi_picontrol_histsoc_co2_dis_global_daily_2001_2005.nc4"
daily_meta <- lpjmlkit::read_cdf_header(nc_in_file = daily_discharge)
daily_data <- lpjmlkit::read_cdf(nc_in_file = daily_discharge, nc_header = daily_meta)


# test a variable name with "lat" in name (that might confuse the latitude 
# reading) -> no the dims are separate from the variable names, so unless the 
# time dimension has lat/lon in it, this is working perfectly fine!
volat_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/mn_volatilization.nc"
volat_meta <- lpjmlkit::read_cdf_header(nc_in_file = volat_file)
volat_data <- lpjmlkit::read_cdf(nc_in_file = volat_file, nc_header = volat_meta)

# test a file with timesteps
devtools::load_all("/p/projects/open/Fabian/LPJbox/lpjmlkit_read_cdf/")
soilc_timestep_file <- "/p/tmp/sibylls/Methane/output_holocene/soilc.nc"
timestep_meta <- lpjmlkit::read_cdf_header(nc_in_file = soilc_timestep_file)
timestep_data <- lpjmlkit::read_cdf(nc_in_file = soilc_timestep_file, 
                                    nc_header = timestep_meta,
                                    subset = list(year = c("-129850","-129750")))

# test meta reading
fpc_file_meta <- "/p/projects/open/Fabian/runs/metrics_cdf/output/lu_1500_2016/fpc.nc.json"
fpc_meta_read <- read_io(filename = fpc_file_meta)

# testing read_io
fpc_file <- "/p/projects/open/Fabian/runs/metrics_cdf/output/lu_1500_2016/fpc.nc"
data <- read_io(filename = fpc_file)

# directly calling read_io with subsetting does not work yet ...
read_data <- read_io(filename = fpc_file, 
                     subset = list(year = as.character(2000:2005), 
                          band = c("natural stand fraction", 
                                    "tropical broadleaved evergreen tree")))
