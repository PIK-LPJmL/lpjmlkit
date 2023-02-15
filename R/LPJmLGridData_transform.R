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

      # Calculate grid extent from range to span raster. Separate calls for min
      # and max are much faster than one call for range on large grid arrays.
      grid_extent <- rbind(
        min = apply(self$data, "band", min),
        max = apply(self$data, "band", max)
      )


      # Generate two-dimensional array covering the full grid_extent with
      # lon_lat dimensions using orientation as in raster objects, i.e. from
      # west to east and from north to south.
      spatial_dimnames <- mapply( # nolint:undesirable_function_linter.
        seq, # nolint:undesirable_function_linter.
        from = grid_extent[cbind(c("min", "max"), c("lon", "lat"))],
        to = grid_extent[cbind(c("max", "min"), c("lon", "lat"))] +
          c(private$.meta$cellsize_lon, private$.meta$cellsize_lat) *
          c(0.5, -0.5),
        by = c(private$.meta$cellsize_lon, -private$.meta$cellsize_lat),
        SIMPLIFY = FALSE
      )
      spatial_dimnames <- lapply(spatial_dimnames, format, trim = TRUE,
                                 scientific = FALSE, SIMPLIFY = FALSE) # nolint:undesirable_function_linter.
      names(spatial_dimnames) <- c("lon", "lat")

      # Initialize grid array
      grid_array <- array(NA,
                          dim = lapply(spatial_dimnames, length),
                          dimnames = spatial_dimnames)

      # Get indices of lat and lon dimnames
      ilon <- round(
        (asub(self$data, band = "lon") - min(grid_extent[, "lon"])) /
          private$.meta$cellsize_lon
      ) + 1
      # ilat in descending order
      ilat <- round(
        (max(grid_extent[, "lat"]) - asub(self$data, band = "lat")) /
          private$.meta$cellsize_lat
      ) + 1

      # Set dimnames of grid_array to actual coordinates instead of dummy
      # spatial_dimnames.
      coordlon <- asub(self$data, band = "lon")[
        match(seq_len(dim(grid_array)["lon"]), ilon)
      ]
      coordlat <- asub(self$data, band = "lat")[
        match(seq_len(dim(grid_array)["lat"]), ilat)
      ]

      dimnames(grid_array)$lon <- ifelse(
        is.na(coordlon),
        dimnames(grid_array)$lon,
        coordlon
      )
      dimnames(grid_array)$lat <- ifelse(
        is.na(coordlat),
        dimnames(grid_array)$lat,
        coordlat
      )

      # Replace cell of lon and lat by cell index
      grid_array[cbind(ilon, ilat)] <- as.integer(dimnames(self$data)$cell)

      self$.__set_data__(grid_array)

      private$.meta$.__transform_space_format__("lon_lat")

    # Case 2: Transformation from lon, lat dimensions to cell dimension
    } else if (private$.meta$._space_format_ == "lon_lat" &&
        to == "cell") {

      # Get ilon and ilat of non-missing cells
      grid_indices <- which(!is.na(self$data), arr.ind = TRUE)

      # Get corresponding cell index
      cell_indices <- self$data[grid_indices]

      # Get lon and lat coordinate values corresponding to each ilon/ilat value
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
          dimnames = list(
            cell = format(self$data[grid_indices], trim = TRUE,
                          scientific = FALSE),
            band = c("lon", "lat")
          )
        )[order(cell_indices), ]
      )

      private$.meta$.__transform_space_format__("cell")
    }

    return(invisible(self))
  }
)
