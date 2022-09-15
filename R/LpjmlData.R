#' LPJmL data class
#'
#' Handles LPJmL output and input data
#'
#' @param data_array LPJmL data array

#' @param meta_data LpjmlMetaData Object
#'
#' @return LpjmlData object
#'
#' @examples
#' \dontrun{
#' }
#' @export
# https://adv-r.hadley.nz/r6.html#r6-classes, also why CamelCase is used ...
LpjmlData <- R6::R6Class(
  classname = "LpjmlData",
  lock_objects = FALSE,
  public = list(
    meta_data = NULL,
    data = NULL,
    # init function
    initialize = function(data_array, meta_data = NULL) {
      if (methods::is(meta_data, "LpjmlMetaData") |
          methods::is(meta_data, "NULL")) {
        self$meta_data <- meta_data
      } else {
        stop("Provide a LpjmlMetaData object for meta_data.")
      }
      self$data <- data_array
      if (!is.null(self$meta_data$variable)) {
        if (self$meta_data$variable == "grid") {
          private$init_grid()
        }
      }
    },
    as_tibble = function(subset_list = NULL, value_name = "value") {
      self$data %>%
        subset_array(subset_list) %>%
        reshape2::melt(value.name = value_name) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(across(names(dimnames(self$data)), as.factor)) %>%
        return()
    },
    as_array = function(subset_list = NULL,
                        # spatial_format = c("cell", "lon_lat")
                        spatial_format = NULL,
                        # time_format = c("time", "year_month_day")
                        time_format = NULL) {
      # initiate clone to be returned on which following methods are executed
      x <- self$clone(deep = TRUE)

      # convert spatial dimension format
      if (!is.null(spatial_format)) {
        x <- convert_spatial(x, spatial_format)
      }

      # convert time dimension format
      if (!is.null(time_format)) {
        x <- convert_time(x, time_format)
      }
      # apply subset
      x$subset(subset_list)

      return(x$data)
    },
    as_raster = function(subset_list = NULL,
                         aggregate_dim = NULL,
                         aggregate_fun = sum,
                         aggregate_idx = c(),
                         ...) {
      if (!is.null(self$meta_data$variable) &&
          self$meta_data$variable == "grid" &&
          self$meta_data$dimspatial_format == "cell") {
        stop(paste("not legit for variable", self$meta_data$variable))
      }
      # support of lazy loading of grid for meta files else add explicitly
      if (is.null(self$grid) &&
          self$meta_data$dimspatial_format == "cell") {
        self$add_grid()
      }
      # workflow adjusted for subsetted grid (via cell)
      data_subset <- subset(self, subset_list)
      if (!is.null(aggregate_dim) &&
          !any(aggregate_dim %in% strsplit(self$meta_data$dimspatial_format,"_")[[1]]) && # nolint
          all(aggregate_dim %in% names(dim(self$data)))) {
        # not recommended for self, some meta_data not valid for data_subset!
        data_subset$data <- aggregate(self,
                                      dimension = aggregate_dim,
                                      fun = aggregate_fun,
                                      dim_subset = aggregate_idx,
                                      ...)
      } else if (!is.null(aggregate_dim)) {
        stop(paste("Only non-spatial and existing dimensions are valid for",
                   "argument aggregate_dim. Please adjust",
                   toString(dQuote(aggregate_dim))))
      }
      # calculate grid extent from range to span raster
      if (data_subset$meta_data$dimspatial_format == "cell") {
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
        c(-data_subset$meta_data$cellsize_lon / 2,
          data_subset$meta_data$cellsize_lon / 2,
          -data_subset$meta_data$cellsize_lat / 2,
          data_subset$meta_data$cellsize_lat / 2),
        nrow = 2,
        ncol = 2
      )

      tmp_raster <- raster::raster(
        res = c(data_subset$meta_data$cellsize_lon,
                data_subset$meta_data$cellsize_lat),
        xmn = grid_extent[1, 1],
        xmx = grid_extent[2, 1],
        ymn = grid_extent[1, 2],
        ymx = grid_extent[2, 2],
        crs = "EPSG:4326"
      )
      # get dimensions larger 1 to check if raster or brick required
      #   (or too many dimensions > 1 which are not compatible with raster)
      multi_dims <- names(which(dim(data_subset$data) > 1))
      # check for dimspatial_format == "lon_lat" if multiple bands/time convert
      #   to "cell" format, if larger stop
      if (data_subset$meta_data$dimspatial_format == "lon_lat") {
        if (length(multi_dims) == 3) {
          data_subset$convert_space()
          multi_dims <- names(which(dim(data_subset$data) > 1))
        } else if (length(multi_dims) > 3) {
          stop(
            paste("Too many dimensions with length > 1.",
                  "Reduce to max. two dimensions via subset function or",
                  "argument.")
          )
        } else {
          tmp_raster <- raster::raster(
            data_subset$data[rev(seq_len(dim(data_subset$data)[["lat"]])), ],
            template = tmp_raster
          )
          names(tmp_raster) <- data_subset$meta_data$variable
        }
      }
      if (data_subset$meta_data$dimspatial_format == "cell") {
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
          names(tmp_raster) <- data_subset$meta_data$variable
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
    },
    as_rast = function(grid_file, as_layers = "band", subset_list = NULL) {
      stop("TO BE IMPLEMENTED SOON")
    },
    length = function() {
      return(length(self$data))
    },
    dim = function() {
      dim(self$data)
    },
    dimnames = function() {
      dimnames(self$data)
    },

    # INSERT ROXYGEN SKELETON: SUBSET METHOD
    subset = function(subset_list) {
      # if ("coords" %in% names(subset_list) &&
      #     self$meta_data$dimspatial_format == "coords") {
      #   if (is.matrix(subset_list["lon_lat"])) {
      #     for (irow in seq_len(subset_list["coords"])) {
      #       self$data[irow[1], irow[2], , ]
      #     }
      #   }
      # init reduced subset list to avoid later double or wrong subsetting
      reduced_subset_list <- subset_list

      # spatial subsseting
      lon_lat <- c("lon", "lat")
      dimspatial_format <- self$meta_data$dimspatial_format

      # cell subset before dims are converted to lat, lon
      if ("cell" %in% names(subset_list) &&
          !any(lon_lat %in% names(subset_list)) &&
          self$meta_data$dimspatial_format != "cell") {
        self$convert_spatial("cell")
        self$data <- subset_array(self$data,
                                  subset_list["cell"],
                                  drop = FALSE)
        # update reduced_subset_list to avoid later double or wrong subsetting
        reduced_subset_list["cell"] <- NULL
        self$convert_spatial(dimspatial_format)

      # lat, lon subset before dims are converted to cell
      } else if (any(lon_lat %in% names(subset_list)) &&
                 !("cell" %in% names(subset_list)) &&
                 self$meta_data$dimtime_format != "lon_lat") {
        self$convert_time("lon_lat")
        name_idx <- as.vector(
          na.omit(match(lon_lat, names(subset_list)))
        )
        self$data <- subset_array(self$data,
                                  subset_list[name_idx],
                                  drop = FALSE)
        # subset grid & update corresponding meta data
        self$grid$data <- subset_array(self$data, subset_list[name_idx],
                                       drop = FALSE)
        self$grid$meta_data$._update_subset(subset_list[name_idx])
        # update reduced_subset_list to avoid later double or wrong subsetting
        reduced_subset_list[name_idx] <- NULL
        self$convert_spatial(dimspatial_format)

      } else if (any(lon_lat %in% names(subset_list)) &&
                 "cell" %in% names(subset_list)) {
        stop(paste("Combinations of subset dimension \"cell\" with dimensions",
                  toString(dQuote(lon_lat)),
                  "are not allowed."))
      }

      # time subsseting
      year_month_day <- c("year", "month", "day")
      dimtime_format <- self$meta_data$dimtime_format

      # for time if dimtime_format is not time convert_time format
      if ("time" %in% names(subset_list) &&
          !any(year_month_day %in% names(subset_list)) &&
          self$meta_data$dimtime_format != "time") {
        self$convert_time("time")
        self$data <- subset_array(self$data,
                                  subset_list["time"],
                                  drop = FALSE)
        reduced_subset_list["time"] <- NULL
        self$convert_time(dimtime_format)

      # for year, month, day if dimtime_format is not year, month, day
      #   convert_time format
      } else if (any(year_month_day %in% names(subset_list)) &&
                 !("time" %in% names(subset_list)) &&
                 self$meta_data$dimtime_format != "year_month_day") {
        self$convert_time("year_month_day")
        name_idx <- as.vector(
          na.omit(match(year_month_day, names(subset_list)))
        )
        self$data <- subset_array(self$data,
                                  subset_list[name_idx],
                                  drop = FALSE)
        reduced_subset_list[name_idx] <- NULL
        self$convert_time(dimtime_format)


      } else if (any(year_month_day %in% names(subset_list)) &&
                 "time" %in% names(subset_list)) {
        stop(paste("Combinations of subset dimension \"time\" with dimensions",
                  toString(dQuote(year_month_day)),
                  "are not allowed."))
      }

      # rest of subsetting (with reduced subset_list that has not been subset)
      self$data <- subset_array(self$data, reduced_subset_list, drop = FALSE)

      if ("time" %in% names(subset_list) &&
          self$meta_data$dimtime_format == "time") {
        self$meta_data$._update_subset(subset_list, self$dimnames()[["time"]])
      } else {
        self$meta_data$._update_subset(subset_list)
      }
      if (!is.null(self$grid) && !is.null(subset_list[["cell"]])) {
        self$grid$data <- subset_array(self$data, subset_list["cell"],
                                       drop = FALSE)
        self$grid$meta_data$._update_subset(subset_list["cell"])
      }
      return(invisible(self))
    },

    # INSERT ROXYGEN SKELETON: ADD GRID METHOD
    add_grid = function(...) {
      if (!is.null(self$meta_data$variable) &&
          self$meta_data$variable == "grid") {
        stop(paste("not legit for variable", self$meta_data$variable))
      }
      # check if meta file for grid is located in output location
      grid_file <- list.files(self$meta_data$data_dir,
                              pattern = "grid.bin.json")
      if (length(grid_file) == 1) {
        # if so get concatenate existing file and data_dir to read grid
        filename <- paste(self$meta_data$data_dir, grid_file, sep = "/")
        # add support for cell subsets - this is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now
        if (self$meta_data$subset_spatial) {
          self$grid <- read_io(
            file_name = filename,
            subset_list = list(cell = self$dimnames()[["cell"]])
          )
        } else {
          self$grid <- read_io(file_name = filename)
        }
      } else {
        # all arguments have to be provided manually via read_io args
        #   ellipsis (...) does that
        # check if arguments are provided
        if (length(as.list(match.call())) > 1) {
          self$grid <- read_io(...)
        } else {
          stop(paste("If no meta file is available $add_grid",
                     "has to be called explicitly with args as read_io."))
        }
      }
      return(invisible(self))
    },

    convert_grid = function(dim_format = NULL) {
      if (is.null(self$meta_data$variable) ||
          self$meta_data$variable != "grid") {
        stop(paste("not legit for variable", self$meta_data$variable))
      }
      # convenience function - if null automatically switch to other dim_format
      if (is.null(dim_format)) {
        if (self$meta_data$dimspatial_format == "cell") {
          dim_format <- "lon_lat"
        } else {
          dim_format <- "cell"
        }
      }

      # convert between single cell dimension and lon, lat dimensions
      if (self$meta_data$dimspatial_format == "cell" &&
          dim_format == "lon_lat") {
        # calculate grid extent from range to span raster
        grid_extent <- apply(
            self$data,
            "band",
            range
        )
        # calculate dimnames for full 2 dimensional grid
        spatial_dimnames <- mapply(seq,
                                   rev(grid_extent[1, ]),
                                   rev(grid_extent[2, ]),
                                   by = c(self$meta_data$cellsize_lat,
                                          self$meta_data$cellsize_lon),
                                   SIMPLIFY = FALSE)
        # init grid array
        grid_array <- array(NA,
                          dim = lapply(spatial_dimnames, length),
                          dimnames = spatial_dimnames)
        # get indices of lat and lon dimnames
        ilon <- match(self$data[, 1],
                     as.numeric(dimnames(grid_array)[["lon"]]))
        ilat <- match(self$data[, 2],
                     as.numeric(dimnames(grid_array)[["lat"]]))
        # replace cell of of lon and lat by cell index
        grid_array[cbind(ilat, ilon)] <- as.integer(
          dimnames(self$data)[["cell"]]
        )
        self$data <- grid_array
        self$meta_data$._convert_dimspatial_format("lon_lat")

      # convert between lon, lat dimensions and single cell dimension
      } else if (self$meta_data$dimspatial_format == "lon_lat" &&
          dim_format == "cell") {
        # get indices of actual cells
        grid_indices <- which(!is.na(self$data), arr.ind = TRUE)
        grid_dimnames <- lapply(dimnames(self$data), as.numeric)
        # select actual cells latitude and longitude and set cells as dimnames
        self$data <- array(
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
        self$meta_data$._convert_dimspatial_format("cell")
      }

      return(invisible(self))
    },

    # INSERT ROXYGEN SKELETON: CONVERT SPATIAL METHOD
    # dim_format = c("lon_lat", "cell")
    convert_space = function(dim_format = NULL) {
      if (!is.null(self$meta_data$variable) &&
          self$meta_data$variable == "grid") {
        self$convert_grid(dim_format = dim_format)
        return(invisible(self))
      }
      # support of lazy loading of grid for meta files else add explicitly
      if (is.null(self$grid)) {
        self$add_grid()
      }
      if (is.null(dim_format)) {
        if (self$meta_data$dimspatial_format == "cell") {
          dim_format <- "lon_lat"
        } else {
          dim_format <- "cell"
        }
      }

      # create new data array based on disaggregated time dimension
      other_dimnames <- dimnames(self$data) %>%
        `[<-`(unlist(strsplit(self$meta_data$dimspatial_format, "_")), NULL)
      other_dims <- dim(self$data) %>%
        `[`(names(other_dimnames))

      # convert between single cell dimension and lon, lat dimensions
      if (self$meta_data$dimspatial_format == "cell" &&
          dim_format == "lon_lat") {
        self$grid$convert_grid(dim_format = dim_format)
        data_array <- array(
          self$grid$data,
          dim = c(dim(self$grid$data), other_dims),
          dimnames = do.call(list,
                             args = c(dimnames(self$grid$data),
                                      other_dimnames))
        )
        data_array[!is.na(data_array)] <- self$data
        # set corresponding meta_data entry
        self$meta_data$._convert_dimspatial_format("lon_lat")

      # convert between lon, lat dimensions and single cell dimension
      } else if (self$meta_data$dimspatial_format == "lon_lat" &&
          dim_format == "cell") {
        mask_array <- array(self$grid$data,
                            dim = dim(self$data),
                            dimnames = dimnames(self$data))
        self$grid$convert_grid(dim_format = dim_format)
        data_array <- array(
          NA,
          dim = c(dim(self$grid$data)["cell"], other_dims),
          dimnames = do.call(list,
                             args = c(dimnames(self$grid$data)["cell"],
                                      other_dimnames))
        )
        data_array[] <- self$data[!is.na(mask_array)]
        # set corresponding meta_data entry
        self$meta_data$._convert_dimspatial_format("cell")
        if (!is.null(self$grid)) {
          self$grid$meta_data$._convert_dimspatial_format("cell")
        }

      } else {
        return(invisible(self))
      }
      # overwrite internal data with same data but new dimensions
      self$data <- data_array

      return(invisible(self))
    },

    # INSERT ROXYGEN SKELETON: CONVERT TIME METHOD
    # dim_format = c("year_month_day", "time")
    convert_time = function(dim_format = NULL) {
      if (!is.null(self$meta_data$variable) &&
          self$meta_data$variable == "grid") {
        stop(paste("not legit for variable", self$meta_data$variable))
      }
      if (is.null(dim_format)) {
        if (self$meta_data$dimtime_format == "time") {
          dim_format <- "year_month_day"
        } else {
          dim_format <- "time"
        }
      }

      # convert between aggregated time = "year-month-day" & disaggregated time
      #   format with year, month, day
      # convert from "time" to "year_month_day"
      if (self$meta_data$dimtime_format == "time" &&
          dim_format == "year_month_day") {
        # possible ndays of months
        ndays_in_month <- c(31, 30, 28)
        # split time string "year-month-day" into year, month, day int vector
        time_dimnames <- split_time_names(self$dimnames()[["time"]])

        # assume no daily data - remove day dimension
        if (all(time_dimnames[["day"]] %in% ndays_in_month)) {
          time_dimnames[["day"]] <- NULL
        }
        # assume no monthly data - remove month dimension
        if (length(time_dimnames$month) == 1 &&
            is.null(time_dimnames[["day"]])) {
          time_dimnames[["month"]] <- NULL
        }
        self$meta_data$._convert_dimtime_format("year_month_day")

      # convert from "year_month_day" to "time"
      } else if (self$meta_data$dimtime_format == "year_month_day" &&
                 dim_format == "time") {
        pre_dimnames <- self$dimnames() %>%
          lapply(as.integer)
        time_dimnames <- list(
          time = create_time_names(nstep = self$meta_data$nstep,
                                   years = pre_dimnames$year,
                                   months = pre_dimnames$month,
                                   days = pre_dimnames$day)
        )
        self$meta_data$._convert_dimtime_format("time")
      # else return without execution
      } else {
        return(invisible(self))
      }

      spatial_dims <- unlist(strsplit(self$meta_data$dimspatial_format, "_"))
      # create new data array based on disaggregated time dimension
      time_dims <- lapply(time_dimnames, length)
      self$data <- array(
        self$data,
        dim = c(dim(self$data)[spatial_dims],
                time_dims,
                dim(self$data)["band"]),
        dimnames = do.call(list,
                           args = c(dimnames(self$data)[spatial_dims],
                                    time_dimnames,
                                    dimnames(self$data)["band"]))
      )
      return(invisible(self))
    },
    # aggregation function, only to be applied for conversions (as_raster, plot)
    #   do not apply to self to not violate data integrity !
    aggregate = function(dimension = "band",
                         fun = sum,
                         dim_subset = c(),
                         ...) {
      if (length(dim_subset) > 0) {
        agg_subset <- subset_array(self$data,
                                   structure(list(dim_subset), names = dimension[1]), # nolint
                                   drop = FALSE)
      } else {
        agg_subset <- self$data
      }
      dim_names <- names(dim(agg_subset))
      apply(X = agg_subset,
            MARGIN = dim_names[!dim_names %in% dimension],
            FUN = fun,
            ...) %>%
      return()
    },
    summary = function(dimension = "band",
                       subset_list = NULL,
                       cutoff = FALSE,
                       ...) {
      data <- subset_array(self$data, subset_list)
      if (dimension %in% names(dimnames(data)) &&
          length(which(dim(data) > 1)) > 1) {
        mat_sum <- data %>%
          apply(dimension, c)
        if (dim(mat_sum)[2] > 16 && cutoff) {
          cat(paste0(
            "\u001b[33;3m",
            "Note: not printing all ",
            dimension,
            "s summary, use $summary() or summary() to get all.",
            "\u001b[0m",
            "\n")
          )
          mat_sum[, seq_len(16)] %>%
            summary(...)
        } else {
          if (!is.null(self$meta_data$variable) &&
              self$meta_data$variable == "grid") {

            mat_sum %>%
                summary(...) %>%
                `[`(c(1, 6), )
          } else {
            mat_sum %>%
              summary(...)
          }
        }
      } else {
        mat_sum <- summary(matrix(data), ...)
        if (!is.null(self$meta_data$variable) &&
            self$meta_data$variable == "grid") {
          var_name <- "cell"
          mat_sum <- mat_sum[c(1, 6), ]
        } else {
          var_name <- default(self$meta_data$variable, "")
        }
        space_len <- ifelse(nchar(var_name) > 8,
                            0,
                            4 - sqrt(nchar(var_name)))
        paste0(c(rep(" ", space_len), var_name, "\n")) %>%
          append(paste0(mat_sum, collapse = "\n ")) %>%
        cat()
        return(bquote())
      }
    },
    print = function() {
      quotes_option <- options(useFancyQuotes = FALSE)
      on.exit(options(quotes_option))
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      cat(paste0("\u001b[1m", blue_col, "$meta_data %>%", unset_col, "\n"))
      self$meta_data$print(all = FALSE, spaces = "  .")
      cat(paste0("\u001b[33;3m",
                 "Note: not printing all meta data, use $meta_data to get all.",
                 unset_col,
                 "\n"))
      if (!is.null(self$grid)) {
        cat(paste0("\u001b[1m\u001b[31m",
                   "$grid",
                   unset_col,
                   "\u001b[31m",
                   " ...",
                   unset_col,
                   "\n"))
      }
      cat(paste0("\u001b[1m",
                 blue_col,
                 "$data %>%",
                 unset_col,
                 "\n"))
      dim_names <- self$dimnames()
      cat(paste0(blue_col, "  dimnames() %>%", unset_col, "\n"))
      for (sub in seq_along(dim_names)) {
        to_char2 <- ifelse(is.character(dim_names[[sub]]), "\"", "")
        if (length(dim_names[[sub]]) > 6) {
          abbr_dim_names <- paste0(c(paste0(to_char2,
                                            dim_names[[sub]][1:4],
                                            to_char2),
                                   "...",
                                   paste0(to_char2,
                                          utils::tail(dim_names[[sub]], n = 1),
                                          to_char2)))
        } else {
          abbr_dim_names <- paste0(to_char2, dim_names[[sub]], to_char2)
        }
        cat("  ",
            blue_col,
            paste0(".$", names(dim_names[sub])),
            unset_col,
            abbr_dim_names)
        cat("\n")
      }
      cat(paste0(blue_col, "$summary()", unset_col, "\n"))
      print(self$summary(cutoff = TRUE))
      if (is.null(self$meta_data$variable) ||
      self$meta_data$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )
      } else {
        cat(paste0("\u001b[33;3m",
                   ifelse(self$meta_data$dimspatial_format == "cell",
                          "Note: only min & max printed as equivalent to spatial extent.", #nolint
                          "Note: inverted grid (cell as value)! Only min & max printed for sequence of cells."), #nolint
                   unset_col,
                   "\n"))
      }
    }
  ),
  private = list(
    init_grid = function() {
      # update grid data
      dimnames(self$data)[["band"]] <- c("lon", "lat")
      # update grid meta data
      self$data <- drop_omit(self$data, omit = "cell")
      self$meta_data$._init_grid()
      return(invisible(self))
    }
  )
)

# set up method dispatch
#   https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class

# overwrite S3 methods
length.LpjmlData <- function(x, ...) x$length(...)
dim.LpjmlData <- function(x, ...) x$dim(...)
dimnames.LpjmlData <- function(x, ...) x$dimnames(...)
summary.LpjmlData <- function(x, ...) x$summary(...)
subset.LpjmlData <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$subset(...)
  return(y)
}

# add additional (important) functions for method dispatch with deep copying
#   x, execute function on copied object and return ("traditional way")
add_grid <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$add_grid(...)
  return(y)
}

convert_time <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$convert_time(...)
  return(y)
}

aggregate <- function(x, ...) {
  y <- x$aggregate(...)
  return(y)
}

convert_space <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$convert_space(...)
  return(y)
}

as_raster <- function(x, ...) {
  y <- x$as_raster(...)
  return(y)
}

as_tibble <- function(x, ...) {
  y <- x$as_tibble(...)
  return(y)
}

as_array <- function(x, ...) {
  y <- x$as_array(...)
  return(y)
}
