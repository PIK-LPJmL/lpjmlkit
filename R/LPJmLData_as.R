#' Coerce LPJmLData to an array
#'
#' Function to coerce (convert) a `LPJmLData` object into a pure
#' `link[base]{array}`. Pure - because LPJmLData stores the data already as 
#' an `array` which can be accessed via `$data`.
#' `as_array` provides further functionality to subset or aggregate the `array`.
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`, more information at
#' \link[lpjmlKit]{subset}.
#'
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param ... arguments forwarded to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return a `link[base]{array}` with dimensions of objects `$data` with applied
#' `subset` and `aggregate` functionality as well as `dim` and `dimnames` from
#' the `LPJmLData` object.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # returns array attribute of LPJmLData object directly
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
#' # returns one dimensional array with timeseries for cells `27410:27415`
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
as_array <- function(x, ...) {
  y <- x$as_array(...)
  return(y)
}

LPJmLData$set("public",
              "as_array",
              function(subset = NULL,
                       aggregate = NULL,
                       ...) {
    # initiate clone to be returned on which following methods are executed
    data_subset <- self$clone()
    `if`(!is.null(subset),
         do.call(data_subset$subset, args = subset),
         data_subset) %>%
      aggregate_array(aggregate_list = aggregate,
                      ...) %>%
      return()
  }
)


#' Coerce LPJmLData to an tibble
#'
#' Function to coerce (convert) a `LPJmLData` object into a
#' \link[tibble]{tibble} (modern `link[base]{data.frame}`, read more
#' [here](https://r4ds.had.co.nz/tibbles.html))
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`, more information at
#' \link[lpjmlKit]{subset}.
#'
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param value_name rename value column in  returned `tibble`. Defaults to
#' `"value"`.
#'
#' @param ... arguments forwarded to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return a `link[tibble]{tibble}` with columns corresponding to dimension
#' naming of `LPJmLData$data` array and values in value column.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # returns one dimensional array with timeseries for cells `27410:27415`
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
as_tibble <- function(x, ...) {
  y <- x$as_tibble(...)
  return(y)
}

LPJmLData$set("public",
              "as_tibble",
              function(subset = NULL,
                       aggregate = NULL,
                       value_name = "value",
                       ...) {
    data <- self %>%
      as_array(subset, aggregate, ...)

    data %>%
      reshape2::melt(value.name = value_name) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(across(names(dimnames(data)), as.factor)) %>%
      return()
  }
)


#' Coerce LPJmLData to a raster object
#'
#' Function to coerce (convert) a `LPJmLData` object into a
#' \link[raster]{raster} or \link[raster]{brick} object, that opens the space
#' for any GIS based raster operations. Read more
#' [here](https://rspatial.github.io/raster/reference/raster-package.html). The
#' successor package of raster is [terra](https://rspatial.org/), a coercion
#' method will follow soon.
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`, more information at
#' \link[lpjmlKit]{subset}.
#'
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param ... arguments forwarded to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return a `link[raster]{raster}` or `link[raster]{brick}` with grid based on
#' internal `$grid` attribute (LPJmLData of `"./grid.*"`) and corresponding data
#' for each grid cell. If multiple bands or time dimensions exist, a
#' `link[raster]{brick}` is created. Further meta information such as the
#' lon/lat resolution are extracted from `$meta`.
#'
#' @details for coercion to a `link[raster]{raster}` or `link[raster]{brick}`,
#' the `$grid` attribute is required. When using `file_type = "meta"`, grid data
#' is read automatically via `\link[lpjmlKit]{add_grid}`. If `file_type = "clm"`
#' or `file_type = "raw"` is used, `add_grid` has to be performed beforehand.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # returns a raster brick for all data
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
as_raster <- function(x, ...) {
  y <- x$as_raster(...)
  return(y)
}

LPJmLData$set("public",
              "as_raster",
              function(subset = NULL,
                       aggregate = NULL,
                       ...) {
    if (!is.null(self$meta$variable) &&
        self$meta$variable == "grid" &&
        self$meta$space_format == "cell") {
      stop(paste("not legit for variable", self$meta$variable))
    }
    # support of lazy loading of grid for meta files else add explicitly
    if (is.null(self$grid) &&
        self$meta$space_format == "cell") {
      self$add_grid()
    }
    # workflow adjusted for subsetted grid (via cell)
    data_subset <- self$clone()
    if (!is.null(subset)) {
      do.call(data_subset$subset, args = subset)
    }
    if (!is.null(aggregate) &&
        !any(names(aggregate) %in% strsplit(self$meta$space_format,"_")[[1]]) && # nolint
        all(names(aggregate) %in% names(dim(self$data)))) {
      # not recommended for self, some meta data not valid for data_subset!
      data_subset$data <- aggregate_array(data_subset,
                                          aggregate_list = aggregate,
                                          ...)
    } else if (!is.null(aggregate)) {
      stop(paste("Only non-spatial and existing dimensions are valid for",
                 "aggregate. Please adjust",
                 toString(dQuote(names(aggregate)))))
    }
    # calculate grid extent from range to span raster
    if (data_subset$meta$space_format == "cell") {
      data_extent <- apply(data_subset$grid$data,
                           "band",
                           range)
    } else {
      data_extent <- matrix(c(range(as.numeric(dimnames(data_subset$data)[["lon"]])), # nolint
                              range(as.numeric(dimnames(data_subset$data)[["lat"]]))), # nolint
                              nrow = 2,
                              ncol = 2)
    }
    grid_extent <- data_extent + matrix(
      # coordinates represent the centre of cell, for the extent borders
      #   are required, thus subtract/add half of resolution
      c(-data_subset$meta$cellsize_lon / 2,
        data_subset$meta$cellsize_lon / 2,
        -data_subset$meta$cellsize_lat / 2,
        data_subset$meta$cellsize_lat / 2),
      nrow = 2,
      ncol = 2
    )

    tmp_raster <- raster::raster(
      res = c(data_subset$meta$cellsize_lon,
              data_subset$meta$cellsize_lat),
      xmn = grid_extent[1, 1],
      xmx = grid_extent[2, 1],
      ymn = grid_extent[1, 2],
      ymx = grid_extent[2, 2],
      crs = "EPSG:4326"
    )
    # get dimensions larger 1 to check if raster or brick required
    #   (or too many dimensions > 1 which are not compatible with raster)
    multi_dims <- names(which(dim(data_subset$data) > 1))
    # check for space_format == "lon_lat" if multiple bands/time convert
    #   to "cell" format, if larger stop
    if (data_subset$meta$space_format == "lon_lat") {
      if (length(multi_dims) == 3) {
        data_subset$transform_space()
        multi_dims <- names(which(dim(data_subset$data) > 1))
      } else if (length(multi_dims) > 3) {
        stop(
          paste("Too many dimensions with length > 1.",
                "Reduce to max. two dimensions via subset function or",
                "argument.")
        )
      } else {
        tmp_raster <- data_subset$data %>%
          subset_array(list(lat = rev(seq_len(dim(data_subset$data)[["lat"]]))), # nolint
                      force_idx = TRUE) %>%
          raster::raster(template = tmp_raster)
        names(tmp_raster) <- data_subset$meta$variable
      }
    }
    if (data_subset$meta$space_format == "cell") {
      if (length(multi_dims) > 2) {
        stop(
          paste("Too many dimensions with length > 1.",
                "Reduce to max. two dimensions via $subset.")
        )
      } else if (length(multi_dims) == 2) {
        # get dimension with length > 1 which is not cell to use for
        #   layer naming
        multi_layer <- multi_dims[which(multi_dims != "cell")]
        tmp_raster <- raster::brick(tmp_raster,
                                    nl = dim(data_subset$data)[multi_layer])
        names(tmp_raster) <- dimnames(data_subset$data)[[multi_layer]]
      } else if (length(multi_dims) == 1) {
        # for single rasters use variable as layer name
        names(tmp_raster) <- data_subset$meta$variable
      }
      # add values of raster cells by corresponding coordinates (lon, lat)
      tmp_raster[
        raster::cellFromXY(
          tmp_raster,
          cbind(subset_array(data_subset$grid$data, list(band = "lon")),
                subset_array(data_subset$grid$data, list(band = "lat")))
        )
      ] <- data_subset$data
    }
    return(tmp_raster)
  }
)


#' Coerce LPJmLData to a terra object
#'
#' Function to coerce (convert) a `LPJmLData` object into a
#' \link[terra]{rast}, that opens the space for any GIS based raster operations.
#' Read more [here](https://rspatial.org/), **TO BE IMPLEMENTED SOON**
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`, more information at
#' \link[lpjmlKit]{subset}.
#'
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param ... arguments forwarded to the aggregate function(s), e.g.
#' `na.rm = TRUE`.
#'
#' @return a `link[terra]{rast}`  with grid based on
#' internal `$grid` attribute (LPJmLData of `"./grid.*"`) and corresponding data
#' for each grid cell. If multiple bands or time dimensions exist, a
#' `link[raster]{brick}` is created. Further meta information such as the
#' lon/lat resolution are extracted from `$meta`.
#'
#' @details for coercion to a `link[terra]{rast}` the `$grid` attribute is
#' required. When using `file_type = "meta"`, grid data is read automatically
#' via `\link[lpjmlKit]{add_grid}`. If `file_type = "clm"` or
#' `file_type = "raw"` is used, `add_grid` has to be performed beforehand.
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # returns a SpatRaster for all data
#' as_terra(vegc)
#' # ....
#' }
#'
#' @md
#' @export
as_terra <- function(x, ...) {
  y <- x$as_terra(...)
  return(y)
}

LPJmLData$set("public",
              "as_terra",
              function(grid_file,
                       as_layers = "band",
                       subset = NULL) {
    stop("TO BE IMPLEMENTED SOON ...")
  }
)
