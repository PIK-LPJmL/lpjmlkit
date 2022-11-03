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
