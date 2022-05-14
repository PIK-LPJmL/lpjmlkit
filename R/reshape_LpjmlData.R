#' @title Reshape a LpjmlData object into an array with rearranged dimensions
#'
#' @description Function to read LpjmLData object and rearrange its dimensions
#' into a different structure. LpjmLData dimensions are [cell, time, band]. With
#' this function, you can convert that into an array with dimensions
#' [lon, lat, time] (option output_shape = "array_lon-lat-time").
#' @param data_grid An LPJmL grid output in the form of LpjmLData$data
#' @param meta_grid An LPJmL grid output in the form of LpjmLData$meta_data
#' @param data_lpjml An LPJmL output in the form of LpjmLData$data
#' (note: one band only allowed!)
#' @param meta_lpjml An LPJmL output in the form of LpjmLData$meta_data
#' (note: one band only allowed!)
#' @param output_shape The shape of the output array. Options are:
#' "array_lon-lat-time" [lon, lat, time]; (TODO: other shapes to be implemeted)
#' @return
#' @examples
#' @details
#' @seealso
#' @export

reshape_LpjmlData <- function(
  data_grid    = NULL,
  meta_grid    = NULL,
  data_lpjml   = NULL,
  meta_lpjml   = NULL,
  output_shape = "array_lon-lat-time"
) {

  if (output_shape == "array_lon-lat-time") {
    # Want to return an array with dimensions [lon, lat, time]

    # Convert grid array to data.frame
    df_grid <- data.frame(lon = data_grid[, 1, 1],
                          lat = data_grid[, 1, 2])

    # ------------------------------------ #
    # Lon/Lat information
    lon_range <- range(df_grid$lon)
    lat_range <- range(df_grid$lat)

    # set dims for array_out
    lon_range <- range(df_grid$lon)
    lat_range <- range(df_grid$lat)

    # decimals of lon/lat resolution
    ndigits_lon <- nchar(
      unlist(strsplit(x = as.character(meta_grid$cellsize_lon), split="[.]"))[2]
    )
    ndigits_lat <- nchar(
      unlist(strsplit(x = as.character(meta_grid$cellsize_lat), split="[.]"))[2]
    )

    lons <- round(c(
      seq(from = lon_range[1], to = lon_range[2], by = meta_grid$cellsize_lon),
      lon_range[2]
    ), ndigits_lon)
    lats <- round(c(
      seq(from = lat_range[1], to = lat_range[2], by = meta_grid$cellsize_lat),
        lat_range[2]
    ), ndigits_lat)

    # ------------------------------------ #
    # Initialize array_out

    nlon <- length(lons)                         # length of longitude dimension
    nlat <- length(lats)                         # length of latitude dimension
    ntime <- meta_lpjml$nyear * meta_lpjml$nstep # length of time dimension
    dims_ls   <- list(lon = lons, lat = lats, time = dimnames(data_output)$time)

    array_out <- array(NA, dim = c(nlon, nlat, ntime), dimnames = dims_ls)

    # ------------------------------------ #
    # Loop thorugh grid df rows
    for (i in seq_len(nrow(df_grid))) {

      # Get index of lon and lat on the output array
      ilon <- which.min(abs(lons - df_grid$lon[i]))
      ilat <- which.min(abs(lats - df_grid$lat[i]))

      # Extract values from data_lpjml array & store values in array_out
      array_out[ilon, ilat, ] <- data_lpjml[i, , ]
    }

    return(array_out)

  }
}