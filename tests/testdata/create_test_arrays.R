library(lpjmlkit)

# Create test_array
output <- read_io(filename = "../testdata/output/pft_npp.bin.json")
saveRDS(output$as_array(), "../testdata/test_array.rds")

# Create test_array_lonlat
output <- read_io(filename = "../testdata/output/pft_npp.bin.json")
output$transform(to = "lon_lat")
saveRDS(output$data, "../testdata/test_array_lonlat.rds")
