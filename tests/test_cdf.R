#devtools::load_all("/p/projects/open/Fabian/LPJbox/lpjmlkit_read_cdf/")
devtools::load_all("/home/stenzel/lpjmlkit/")

# annual
fpc_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc"
fpc_file_meta <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc.json"

meta <- lpjmlkit::read_cdf_header(nc_in_file = fpc_file)
data <- lpjmlkit::read_cdf(nc_in_file = fpc_file, nc_header = meta)
data_2000_2005 <- lpjmlkit::read_cdf(nc_in_file = fpc_file, nc_header = meta, 
                              subset = list(year = as.character(2000:2005), 
                                            band = c("natural stand fraction", "tropical broadleaved evergreen tree")))

# monthly
mnpp_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/mnpp.nc"
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

read_data <- read_io(filename = fpc_file, 
                     subset = list(year = as.character(2000:2005), 
                          band = c("natural stand fraction", 
                                    "tropical broadleaved evergreen tree")))
