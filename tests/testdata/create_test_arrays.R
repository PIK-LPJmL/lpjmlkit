library(lpjmlkit)

# Create test_array
output <- read_io(filename = "../testdata/output/pft_npp.bin.json")
saveRDS(output$as_array(), "../testdata/test_array.rds")

# Create test_array_lonlat
output <- read_io(filename = "../testdata/output/pft_npp.bin.json")
output$transform(to = "lon_lat")
saveRDS(output$data, "../testdata/test_array_lonlat.rds")

# Create temperature data subset
tas_data <- read_io(
  file.path(
    "/p", "projects", "lpjml", "input", "historical", "ISIMIP3av2", "obsclim",
    "GSWP3-W5E5",
    "tas_gswp3-w5e5_obsclim_1901-2019.clm.json"
  ),
  subset = list(
    year = as.character(2015:2019),
    cell = as.character(10000:10002)
  )
)
tas_header <- tas_data$meta$as_header()
tas_header <- set_header_item(tas_header, version = 4, name = "LPJCLIM")
tas_write_data <- tas_data$data %>% `dim<-`(c(3, 365, 5)) %>% aperm(c(2, 1, 3))
write_header(
  filename = "../testdata/input/tas_gswp3-w5e5_obsclim_2015-2019.clm",
  header = tas_header
)
fp <- file("../testdata/input/tas_gswp3-w5e5_obsclim_2015-2019.clm", "ab")
if (typeof(get_datatype(tas_header)$type) == "integer") {
  writeBin(
    as.integer(round(tas_write_data / get_header_item(tas_header, "scalar"))),
    con = fp,
    size = get_datatype(tas_header)$size,
    endian = get_header_item(tas_header, "endian")
  )
} else if (typeof(get_datatype(tas_header)$type) == "double") {
  writeBin(
    as.double(tas_write_data / get_header_item(tas_header, "scalar")),
    con = fp,
    size = get_datatype(tas_header)$size,
    endian = get_header_item(tas_header, "endian")
  )
} else {
  stop("Unsupported datatype ", get_header_item(tas_header, "datatype"))
}
close(fp)
