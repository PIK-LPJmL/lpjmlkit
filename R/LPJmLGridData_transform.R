# Method to transform the dimensions of a grid array from "cell" to "lon_lat"
# or the other way around
LPJmLGridData$set("private",
                  ".transform_space",
                  function(to = NULL) {

    # Convenience function - if to == NULL automatically switch to other to
    if (is.null(to)) {
      if (private$.meta$._space_format_ == "cell") {
        to <- "lon_lat"
      } else {
        to <- "cell"
      }
    }

    # Case 1: Transformation from cell dimension to lon, lat dimensions
    if (private$.meta$._space_format_ == "cell" &&
        to == "lon_lat") {

      # calculate grid extent from range to span raster
      grid_extent <- apply(
          self$data,
          "band",
          range
      )

      # Calculate dimnames for full 2 dimensional grid
      spatial_dimnames <- mapply(seq, # nolint:undesirable_function_linter.
                                 grid_extent[1, c("lat", "lon")],
                                 grid_extent[2, c("lat", "lon")] +
                                   c(private$.meta$cellsize_lat,
                                     private$.meta$cellsize_lon) / 2,
                                 by = c(private$.meta$cellsize_lat,
                                        private$.meta$cellsize_lon),
                                 SIMPLIFY = FALSE)

      # Initialize grid array
      grid_array <- array(NA,
                          dim = lapply(spatial_dimnames, length),
                          dimnames = spatial_dimnames)

      # Get indices of lat and lon dimnames
      ilon <- round((asub(self$data, band = "lon") -
        min(grid_extent[, "lon"])) / private$.meta$cellsize_lon) + 1
      ilat <- round((asub(self$data, band = "lat") -
        min(grid_extent[, "lat"])) / private$.meta$cellsize_lat) + 1

      # Set dimnames of grid_array to actual coordinates instead of dummy
      # spatial_dimnames
      coordlon <- asub(self$data, band = "lon")[
        match(seq_len(dim(grid_array)["lon"]), ilon)
      ]
      dimnames(grid_array)$lon <- ifelse(
        is.na(coordlon),
        dimnames(grid_array)$lon,
        coordlon
      )
      coordlat <- asub(self$data, band = "lat")[
        match(seq_len(dim(grid_array)["lat"]), ilat)
      ]
      dimnames(grid_array)$lat <- ifelse(
        is.na(coordlat),
        dimnames(grid_array)$lat,
        coordlat
      )

      # Replace cell of lon and lat by cell index
      grid_array[cbind(ilat, ilon)] <- as.integer(
        dimnames(self$data)$cell
      )
      self$.__set_data__(grid_array)
      private$.meta$.__transform_space_format__("lon_lat")

    # Case 2: Transformation from lon, lat dimensions to cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {

      # Get indices of actual cells
      grid_indices <- which(!is.na(self$data), arr.ind = TRUE)
      grid_dimnames <- lapply(dimnames(self$data), as.numeric)

      # Set values to latitude and longitude coordinates and dimnames to cell
      # indices
      self$.__set_data__(
        array(
          cbind(
            lon = grid_dimnames[["lon"]][
              grid_indices[, which(names(dim(self$data)) == "lon")]
            ],
            lat = grid_dimnames[["lat"]][
              grid_indices[, which(names(dim(self$data)) == "lat")]
            ]
          ),
          dim = c(cell = length(self$data[grid_indices]),
                  band = 2),
          dimnames = list(cell = self$data[grid_indices],
                          band = c("lon", "lat"))
        )
      )

      private$.meta$.__transform_space_format__("cell")
    }

    return(invisible(self))
  }
)
