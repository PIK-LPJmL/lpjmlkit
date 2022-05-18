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
    initialize = function(data_array, meta_data = NULL, subset_list) {
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
    as_array = function(subset_list = NULL) {
      self$data %>%
        subset_array(subset_list) %>%
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
    as_raster = function(grid_file, as_layers = "band", subset_list = NULL) {
      stop("TO BE IMPLEMENTED SOON")
    },
    as_brick = function(grid_file, as_layers = "band", subset_list = NULL) {
      stop("TO BE IMPLEMENTED SOON")
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
    add_grid = function(...) {
      grid_file <- list.files(dirname(file_name), pattern = "grid.bin.json")
      if (length(grid_file) == 1) {
        filename <- paste(self$meta_data$data_dir, grid_file, sep = "/")
        self$grid <- read_output(file_name = filename)
      } else {
        self$grid <- read_output(file_name = filename, ...)
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
      cat(paste0(blue_col, "  $dimnames() %>%", unset_col, "\n"))
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
      cat(paste0(blue_col, "  $summary()", unset_col, "\n"))
      print(self$summary(cutoff = TRUE))
      if (self$meta_data$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )
      } else {
        cat("\n")
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