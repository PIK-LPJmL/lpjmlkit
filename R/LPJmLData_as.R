#' Coerce an LPJmLData object to an array
#'
#' Function to coerce (convert) an [`LPJmLData`] object into a pure
#' \link[base]{array}. Pure - because LPJmLData stores the data already as
#' an `array` which can be accessed via `$data`.
#' `as_array` provides additional functionality to subset or aggregate the
#' `array`.
#'
#' @param x [LPJmLData] object.
#'
#' @param subset List of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`. More information at
#' [`subset.LPJmLData()`].
#'
#' @param aggregate List of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param ... Arguments passed to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return an \link[base]{array} with dimensions of object `$data` with
#' applied `subset` and `aggregate` functionality as well as `dim` and
#' `dimnames` from the [`LPJmLData`] object.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Returns array attribute of LPJmLData object directly
#' vegc$data
#' #        time
#' # cell      1901-12-31   1902-12-31   1903-12-31   1904-12-31   1905-12-31
#' #   0     1.362730e+04 1.363163e+04 1.364153e+04 1.365467e+04 1.366689e+04
#' #   1     1.201350e+02 1.158988e+02 1.101675e+02 1.214204e+02 1.062658e+02
#' #   2     1.334261e+02 1.210387e+02 1.218128e+02 1.183210e+02 1.159934e+02
#' #   3     9.744530e+01 9.586801e+01 8.365642e+01 8.193731e+01 7.757602e+01
#' #   4     7.592700e+01 7.821202e+01 6.798551e+01 6.632317e+01 5.691082e+01
#' #   5     1.106748e+01 1.137272e+01 1.196524e+01 1.131316e+01 9.924266e+0
#'
#' # Returns two-dimensional array with timeseries for the mean across cells
#' # 27410:27415
#' as_array(vegc,
#'          subset = list(cell = 27410:27415),
#'          aggregate = list(cell = mean))
#'
#' #             band
#' # time                1
#' #   1901-12-31 1995.959
#' #   1902-12-31 1979.585
#' #   1903-12-31 1978.054
#' #   1904-12-31 1935.623
#' #   1905-12-31 1938.805
#'
#' }
#'
#' @md
#' @export
as_array <- function(x,
                     subset = NULL,
                     aggregate = NULL,
                     ...) {
  y <- x$as_array(subset,
                  aggregate,
                  ...)
  y
}

# as_array method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".as_array",
              function(subset = NULL,
                       aggregate = NULL,
                       ...) {

    # Initiate clone to be returned on which following methods are executed
    data_subset <- self$clone(deep = TRUE)
    `if`(!is.null(subset),
         do.call(data_subset$subset, args = subset),
         data_subset) %>%
      aggregate_array(aggregate_list = aggregate,
                      ...) %>%
      return()
  }
)


#' Coerce an LPJmLData object to a tibble
#'
#' Function to coerce (convert) an [`LPJmLData`] object into a
#' \link[tibble]{tibble} (modern \link[base]{data.frame}). Read more about
#' tibbles at <https://r4ds.had.co.nz/tibbles.html)>.
#'
#' @param x [LPJmLData] object
#'
#' @param subset List of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415))`. More information at
#' [`subset.LPJmLData()`].
#'
#' @param aggregate List of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param value_name Name of value column in returned `tibble`. Defaults to
#' `"value"`.
#'
#' @param ... Arguments passed to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return a \link[tibble]{tibble} with columns corresponding to dimension
#' naming of the `LPJmLData$data` array and values in one value column.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Returns two-dimensional tibble representation of vegc$data.
#' as_tibble(vegc)
#' #   cell  time       band    value
#' #   <fct> <fct>      <fct>   <dbl>
#' # 1 0     1901-12-31 1     13627.
#' # 2 1     1901-12-31 1       120.
#' # 3 2     1901-12-31 1       133.
#' # 4 3     1901-12-31 1        97.4
#' # 5 4     1901-12-31 1        75.9
#' # 6 5     1901-12-31 1        11.1
#'
#' }
#'
#' @md
#' @export
as_tibble <- function(x,
                      subset = NULL,
                      aggregate = NULL,
                      value_name = "value",
                      ...) {
  y <- x$as_tibble(subset,
                   aggregate,
                   value_name,
                   ...)
  y
}

# as_tibble method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".as_tibble",
              function(subset = NULL,
                       aggregate = NULL,
                       value_name = "value",
                       ...) {

    data <- self$as_array(subset, aggregate, ...)

    data %>%
      reshape2::melt(value.name = value_name) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(dplyr::across(names(dimnames(data)), as.factor)) %>%
      return()
  }
)


#' Coerce an LPJmLData object to a Raster* object
#'
#' Function to coerce (convert) an [`LPJmLData`] object into a
#' \link[raster]{raster} or \link[raster]{brick} object that allows for any
#' GIS-based raster operations. Read more about the raster package at
#' <https://rspatial.github.io/raster/reference/raster-package.html>.
#' The successor package of raster is called terra: <https://rspatial.org/>.
#'
#' @param x [LPJmLData] object
#'
#' @param subset List of array dimension(s) as name/key and
#'   corresponding subset vector as value, e.g.`list(cell = c(27411:27415))`.
#'   More information at [`subset.LPJmLData()`].
#'
#' @param aggregate List of array dimension(s) as name/key and
#'   corresponding aggregation function as value, e.g. `list(band = sum)`.
#'
#' @param ... Arguments passed to the aggregate function(s), e.g.
#'   `na.rm = TRUE`.
#'
#' @return
#' A \link[raster]{raster} or \link[raster]{brick} object with spatial extent
#' and coordinates based on internal `$grid` attribute and containing a lon/lat
#' representation of `x$data`. If multiple bands or time steps exist, a
#' \link[raster]{brick} is created. Further meta information such as the
#' lon/lat resolution are extracted from `$meta`.
#'
#' @details
#' The `$grid` attribute is required for spatial transformation. When
#' using `file_type = "meta"`, grid data are usually read automatically via
#' [`add_grid()`] if the grid file is present in the same directory. Otherwise,
#' `add_grid()` has to be called explicitly with the path to a matching grid
#' file. Supports either multiple bands or multiple time steps. Use `subset` or
#' `aggregate` to reduce data with multiple bands and time steps.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Returns a RasterBrick for all data
#' as_raster(vegc)
#' # class      : RasterBrick
#' # dimensions : 280, 720, 201600, 200  (nrow, ncol, ncell, nlayers)
#' # resolution : 0.5, 0.5  (x, y)
#' # extent     : -180, 180, -56, 84  (xmin, xmax, ymin, ymax)
#' # crs        : +proj=longlat +datum=WGS84 +no_defs
#' # source     : memory
#' # names      : X1901.12.31, X1902.12.31, X1903.12.31, X1904.12.31, ...
#' # min values :           0,           0,           0,           0, ...
#' # max values :    28680.72,    28662.49,    28640.29,    28634.03, ...
#'
#' }
#'
#' @md
#' @export
as_raster <- function(x,
                      subset = NULL,
                      aggregate = NULL,
                      ...) {
  y <- x$as_raster(subset,
                   aggregate,
                   ...)
  y
}

# as_raster method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".as_raster",
              function(subset = NULL,
                       aggregate = NULL,
                       ...) {

    data_subset <- private$.subset_raster_data(self, subset, aggregate, ...)

    # Create empty raster to use as base for assigning data
    tmp_raster <- create_tmp_raster(data_subset)

    # Get dimensions larger 1 to check if raster or brick is required
    # (or too many dimensions > 1 which are not compatible with raster)
    # space dims are always included (to work with 1 cell rasters)
    multi_dims <- get_multidims(data_subset)


    # Convert space_format from "lon_lat" to "cell" to allow for non-sequential
    # coordinate axes, i.e. non-continuous subsetting along one or both space
    # dimensions.
    if (data_subset$meta$._space_format_ == "lon_lat") {

      data_subset$transform(to = "cell")

      multi_dims <- get_multidims(data_subset)
    }

    # For space_format "cell" allow one additional dimension
    if (data_subset$meta$._space_format_ == "cell") {

      if (length(multi_dims) > 2) {
        stop(
          paste("Too many dimensions with length > 1.",
                "Reduce to one additional dimension besides space via $subset",
                "or $aggregate.")
        )

      } else if (length(multi_dims) == 2) {

        # Get dimension with length > 1 which is not cell to use for
        #   layer naming
        multi_layer <- multi_dims[which(multi_dims != "cell")]
        tmp_raster <- raster::brick(tmp_raster,
                                    nl = dim(data_subset$data)[multi_layer])

        names(tmp_raster) <- dimnames(data_subset$data)[[multi_layer]]

      } else if (length(multi_dims) == 1) {
        # For single rasters use variable as layer name
        names(tmp_raster) <- data_subset$meta$variable
      }

      # Add values of raster cells by corresponding coordinates (lon, lat)
      tmp_raster[
        raster::cellFromXY(
          tmp_raster,
          cbind(subset_array(data_subset$grid$data, list(band = "lon")),
                subset_array(data_subset$grid$data, list(band = "lat")))
        )
      ] <- aperm(data_subset$data,
                 perm = c("cell", setdiff(names(dim(data_subset)), "cell")))
    }

    return(tmp_raster)
  }
)


#' Coerce an LPJmLData object to a terra object
#'
#' Function to coerce (convert) an [`LPJmLData`] object into a
#' \link[terra]{rast} object that allows GIS-based raster operations.
#' Read more about the terra package at <https://rspatial.org/>.
#'
#' @param x [LPJmLData] object.
#'
#' @param subset List of array dimension(s) as name/key and
#'   corresponding subset vector as value, e.g. `list(cell = c(27411:27415)`.
#'   More information at [`subset.LPJmLData()`].
#'
#' @param aggregate List of array dimension(s) as name/key and
#'   corresponding aggregation function as value, e.g. `list(band = sum)`.
#'
#' @param ... Arguments passed to the aggregate function(s), e.g.
#'   `na.rm = TRUE`.

#' @return
#' A \link[terra]{rast} object with spatial extent and coordinates based
#' on internal `$grid` attribute and containing a lon/lat representation of
#' `x$data`. Further meta information such as the lon/lat resolution is
#' extracted from `$meta`.
#'
#' @details
#' The `$grid` attribute is required for spatial transformation. When
#' using `file_type = "meta"`, grid data are usually read automatically via
#' [`add_grid()`] if the grid file is present in the same directory. Otherwise,
#' `add_grid()` has to be called explicitly with the path to a matching grid
#' file. Supports either multiple bands or multiple time steps. Use `subset` or
#' `aggregate` to reduce data with multiple bands and time steps.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Returns a SpatRaster for all data
#' as_terra(vegc)
#' # ...
#' }
#'
#' @md
#' @export
as_terra <- function(x,
                     subset = NULL,
                     aggregate = NULL,
                      ...) {
  y <- x$as_terra(subset,
                  aggregate,
                  ...)
  y
}

# as_terra method roxygen documentation in LPJmlData.R
LPJmLData$set("private",
              ".as_terra",
              function(subset = NULL,
                       aggregate = NULL,
                       ...) {

    data_subset <- private$.subset_raster_data(self, subset, aggregate, ...)

    # Create empty SpatRaster to use as base for assigning data
    tmp_rast <- create_tmp_raster(data_subset, is_terra = TRUE)

    # Get dimensions larger 1 to check if single-layer or multi-layer SpatRaster
    # is required (or too many dimensions > 1)
    # space dims are always included (to work with 1 cell SpatRaster)
    multi_dims <- get_multidims(data_subset)

    # Convert space_format from "lon_lat" to "cell" to allow for non-sequential
    # coordinate axes, i.e. non-continuous subsetting along one or both space
    # dimensions.
    if (data_subset$meta$._space_format_ == "lon_lat") {

      data_subset$transform(to = "cell")

      multi_dims <- get_multidims(data_subset)
    }

    # For space_format "cell" allow one additional dimension
    if (data_subset$meta$._space_format_ == "cell") {

      if (length(multi_dims) > 2) {
        stop(
          paste("Too many dimensions with length > 1.",
                "Reduce to one additional dimension besides space via $subset",
                "or $aggregate.")
        )

      } else if (length(multi_dims) == 2) {

        # Get dimension with length > 1 which is not cell to use for
        #   layer naming
        multi_layer <- multi_dims[which(multi_dims != "cell")]

        tmp_rast <- terra::rast(tmp_rast,
                                nl = dim(data_subset$data)[multi_layer])

        names(tmp_rast) <- dimnames(data_subset$data)[[multi_layer]]

        if (multi_layer == "time") {

          # Set time meta data
          terra::time(tmp_rast) <- as.Date(
            dimnames(data_subset$data)[[multi_layer]]
          )

        } else if (multi_layer %in% c("year", "month", "day")) {

          # Set time meta data as integer (year, month, day)
          terra::time(tmp_rast) <- as.integer(
            dimnames(data_subset$data)[[multi_layer]]
          )
        }

      } else if (length(multi_dims) == 1) {

        # For single-layer SpatRaster use variable as layer name
        names(tmp_rast) <- data_subset$meta$variable
      }

      # Add values of raster cells by corresponding coordinates (lon, lat)
      if (is.array(data_subset$data)) {
        tmp_rast[
          terra::cellFromXY(
            tmp_rast,
            cbind(subset_array(data_subset$grid$data, list(band = "lon")),
                  subset_array(data_subset$grid$data, list(band = "lat")))
          )
        ] <- aperm(data_subset$data,
                  perm = c("cell", setdiff(names(dim(data_subset)), "cell"))) %>% # nolint
        drop()
      } else {
        tmp_rast[
          terra::cellFromXY(
            tmp_rast,
            cbind(subset_array(data_subset$grid$data, list(band = "lon")),
                  subset_array(data_subset$grid$data, list(band = "lat")))
          )
        ] <- data_subset$data
      }
    }

    # Assign units (meta data)
    terra::units(tmp_rast) <- data_subset$meta$unit

    return(tmp_rast)
  }
)


LPJmLData$set("private",
              ".subset_raster_data",
              function(self,
                       subset = NULL,
                       aggregate = NULL,
                       ...) {

    # Support lazy loading of grid for meta files. This throws an error if no
    # suitable grid file is detected.
    self$add_grid()

    # Workflow adjusted for subsetted grid (via cell)
    data_subset <- self$clone(deep = TRUE)
    if (!is.null(subset)) {
      do.call(data_subset$subset, args = subset)
    }

    if (!is.null(aggregate) &&
        !any(names(aggregate) %in% strsplit(private$.meta$._space_format_,"_")[[1]]) && # nolint
        all(names(aggregate) %in% names(dim(self$data)))) {

      # Not recommended for self, some meta data not valid for data_subset
      data_subset$.__set_data__(
        aggregate_array(data_subset,
                        aggregate_list = aggregate,
                        ...)
      )

    } else if (!is.null(aggregate)) {
      stop(
        "Only non-spatial and existing dimensions are valid for ",
        "aggregate. Please adjust ",
        toString(dQuote(names(aggregate)))
      )
    }

    return(data_subset)
  }
)


create_tmp_raster <- function(data_subset, is_terra = FALSE) {

  # Calculate grid extent from range to span raster
  if (data_subset$meta$._space_format_ == "cell") {
    data_extent <- rbind(min = apply(data_subset$grid$data, "band", min),
                         max = apply(data_subset$grid$data, "band", max))

  } else {
    data_extent <- cbind(
      lon = range(as.numeric(dimnames(data_subset$data)[["lon"]])),
      lat = range(as.numeric(dimnames(data_subset$data)[["lat"]]))
    )
  }

  grid_extent <- data_extent + cbind(
    # Coordinates represent the centre of the cell. Subtract/add half of
    # resolution to derive cell borders for extent.
    lon = data_subset$meta$cellsize_lon * c(-0.5, 0.5),
    lat = data_subset$meta$cellsize_lat * c(-0.5, 0.5)
  )


  if (is_terra) {
    tmp_raster <- terra::rast(
      res = c(data_subset$meta$cellsize_lon,
              data_subset$meta$cellsize_lat),
      xmin = grid_extent[1, 1],
      xmax = grid_extent[2, 1],
      ymin = grid_extent[1, 2],
      ymax = grid_extent[2, 2],
      crs = "EPSG:4326"
    )

  } else {
    tmp_raster <- raster::raster(
      res = c(data_subset$meta$cellsize_lon,
              data_subset$meta$cellsize_lat),
      xmn = grid_extent[1, 1],
      xmx = grid_extent[2, 1],
      ymn = grid_extent[1, 2],
      ymx = grid_extent[2, 2],
      crs = projstring
    )
  }

  tmp_raster
}


# Get names of dimensions with length > 1. Always include space dimension(s)
# (cell / lon, lat), which are required for use with raster/terra.
get_multidims <- function(x) {
  space_dims <- unlist(strsplit(x$meta$._space_format_, "_"))
  multi_dims <- names(which(dim(x$data) > 1))

  union(multi_dims, space_dims)
}

# "EPSG:4326" is not supported by as CRS by all versions of the raster package.
# Fallback to lonlat projection string
capture.output(
  {
    success <- try(raster::crs("EPSG:4326"))
  },
  type = "message"
)
if (class(success) == "try-error") {
  projstring <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
} else {
  projstring <- "EPSG:4326"
}
