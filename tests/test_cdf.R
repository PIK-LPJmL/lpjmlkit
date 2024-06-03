#devtools::load_all("/p/projects/open/Fabian/LPJbox/lpjmlkit_read_cdf/")
devtools::load_all("/home/stenzel/lpjmlkit/")

fpc_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc"
fpc_file_meta <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc.json"

head <- lpjmlkit::read_cdf_header(nc_in_file = fpc_file)
data <- lpjmlkit::read_cdf(nc_in_file = fpc_file,
                   nc_header = head
                    )

mnpp_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/mnpp.nc"
monthly_head <- lpjmlkit::read_cdf_header(nc_in_file = mnpp_file) # takes veery long to create the LPJmLMeta object
monthly_data <- lpjmlkit::read_cdf(nc_in_file = mnpp_file, nc_header = monthly_head)