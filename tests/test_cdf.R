fpc_file <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc"
fpc_file_meta <- "/p/projects/amazonas/nfan/output/LPJmL57_spitfire_WFDE5-CRU_wscal/output/fpc.nc.json"

lpjmlkit::detect_io_type(filename = fpc_file)

devtools::load_all("/p/projects/open/Fabian/LPJbox/lpjmlkit_read_cdf/")
lpjmlkit::read_io(filename = fpc_file)

