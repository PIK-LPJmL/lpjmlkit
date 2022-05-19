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
      if (is(meta_data, "LpjmlMetaData") | is(meta_data, "NULL")) {
        self$meta_data <- meta_data
      } else {
        stop("Provide a LpjmlMetaData object for meta_data.")
      }
      self$data <- data_array
      if (self$meta_data$variable == "grid") {
        private$init_grid()
      }
    },
    as_array = function(subset_list = NULL, drop = TRUE) {
      self$data %>%
        subset_array(subset_list, drop) %>%
        return()
    },
    as_tibble = function(subset_list = NULL, value_name = "value") {
      # TODO: convert integers of dimnames to character
      self$data %>%
        subset_array(subset_list) %>%
        reshape2::melt(value.name = value_name) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(across(names(dimnames(self$data)), as.factor)) %>%
        return()
    },
    as_raster = function(subset_list = NULL, fix_extent = NULL) {
      # support of lazy loading of grid for meta files else add explicitly
      if (is.null(self$grid)) {
        self$add_grid()
      }
      # workflow adjusted for subsetted grid (via cell)
      grid_subset <- subset_array(self$grid$data,
                                  subset_list["cell"],
                                  drop = FALSE)
      if (is.null(fix_extent)) {
        # calculate grid extent from range to span raster
        grid_extent <- apply(
            grid_subset,
            "band",
            range
          ) + matrix(
            # coordinates represent the centre of cell, for the extent borders
            #   are required, thus subtract/add half of resolution
            c(-self$meta_data$cellsize_lon / 2,
              self$meta_data$cellsize_lon / 2,
              -self$meta_data$cellsize_lat / 2,
              self$meta_data$cellsize_lat / 2),
            nrow = 2,
            ncol = 2
          )
      } else {
        grid_extent <- matrix(
          fix_extent,
          nrow = 2,
          ncol = 2
        )
      }
      tmp_raster <- raster::raster(
        res = c(self$meta_data$cellsize_lon, self$meta_data$cellsize_lat),
        xmn = grid_extent[1, 1],
        xmx = grid_extent[2, 1],
        ymn = grid_extent[1, 2],
        ymx = grid_extent[2, 2],
        crs = "EPSG:4326"
        )
      data_subset <- self$data %>%
        subset_array(subset_list, drop = FALSE)
      # get dimensions larger 1 to check if raster or brick required
      #   (or too many dimensions > 1 which are not compatible with raster)
      multi_dims <- names(which(dim(data_subset) > 1))
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
                                    nl = dim(data_subset)[multi_layer])
        names(tmp_raster) <- dimnames(data_subset)[[multi_layer]]
      } else if (length(multi_dims) == 1) {
        # for single rasters use variable as layer name
        names(tmp_raster) <- self$meta_data$variable
      }
      # add values of raster cells by corresponding coordinates (lon, lat)
      tmp_raster[
        raster::cellFromXY(
          tmp_raster,
          cbind(subset_array(grid_subset, list(band = "lon")),
                subset_array(grid_subset, list(band = "lat")))
        )
      ] <- data_subset

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
    subset = function(subset_list) {
      self$data <- subset_array(self$data, subset_list)
      self$meta_data$update_subset(subset_list)
      if (!is.null(self$grid)) {
        self$data <- subset_array(self$data, subset_list["cell"])
        self$meta_data$update_subset(subset_list["cell"])
      }
    },
    add_grid = function(...) {
      # check if meta file for grid is located in output location
      grid_file <- list.files(self$meta_data$data_dir,
                              pattern = "grid.bin.json")
      if (length(grid_file) == 1) {
        # if so get concatenate existing file and data_dir to read grid
        filename <- paste(self$meta_data$data_dir, grid_file, sep = "/")
        # add support for cell subsets - this is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now
        if (self$meta_data$subset) {
          self$grid <- read_output(
            file_name = filename,
            subset_list = list(cell = self$dimnames()$cell)
          )
        } else {
          self$grid <- read_output(file_name = filename)
        }
      } else {
        # all arguments have to be provided manually via read_output args
        #   ellipsis (...) does that
        # check if arguments are provided
        if (length(as.list(match.call())) > 1) {
          self$grid <- read_output(...)
        } else {
          stop(paste("If no meta file is available $add_grid",
                     "has to be called explicitly with args as read_output."))
        }
      }
    },
    summary = function(dimension="band", subset_list = NULL, cutoff = FALSE) {
      data <- subset_array(self$data, subset_list)
      if (dimension %in% names(dimnames(data))) {
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
            summary()
        } else {
          if (self$meta_data$variable == "grid") {
            mat_sum %>%
                summary() %>%
                `[`(c(1, 6), )
          } else {
            mat_sum %>%
              summary()
          }
        }
      } else {
        mat_sum <- summary(matrix(data))
        space_len <- ifelse(nchar(self$meta_data$variable) > 8,
                            0,
                            4 - sqrt(nchar(self$meta_data$variable)))
        paste0(c(rep(" ", space_len), self$meta_data$variable, "\n")) %>%
          append(paste0(mat_sum, "\n")) %>%
          cat()
      }
    },
    print = function() {
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
                                          tail(dim_names[[sub]], n = 1),
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
      if (self$meta_data$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )
      } else {
        cat(paste0("\u001b[33;3m",
                   "Note: only min & max printed as equivalent to spatial extent.", #nolint
                   unset_col,
                   "\n"))
      }
    }
  ),
  private = list(
    init_grid = function() {
      # update grid data
      self$data <- self$data * self$meta_data$scalar
      dimnames(self$data)$band <- c("lon", "lat")
      # update grid meta data
      self$data <- drop(self$data)
      self$meta_data$._init_grid()
    }
  )
)

# set up method dispatch
#   https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class
`[.LpjmlData`    <- function(obj, ...) obj$`[`(...)
length.LpjmlData <- function(obj, ...) obj$length(...)
dim.LpjmlData <- function(obj, ...) obj$dim(...)
dimnames.LpjmlData <- function(obj, ...) obj$dimnames(...)
summary.LpjmlData <- function(obj, ...) obj$summary(...)

# demo example with dummy data
# meta_data = read_meta("/p/projects/open/Jannes/lpjml/testing/meta/runs/output/lu/soilc_layer.bin.json")
# data_array <- array(1,
#                     dim = c(cell = meta_data$ncell,
#                             year = meta_data$nyear,
#                             band = meta_data$nbands),
#                     dimnames = list(cell = seq(meta_data$firstcell,
#                                                 length.out = meta_data$ncell),
#                                     year = meta_data$firstyear :
#                                             meta_data$lastyear,
#                                     band = meta_data$band_names))
# soilc_layer <- LpjmlData$new(data_array, meta_data)