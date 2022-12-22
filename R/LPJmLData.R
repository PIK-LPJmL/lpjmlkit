#' @title LPJmL data class
#'
#' @description Handles LPJmL output and input data.
#'
LPJmLData <- R6::R6Class(
  classname = "LPJmLData",
  lock_objects = FALSE,
  public = list(
    #' @field `LPJmLMetaData` object to store corresponding meta data
    meta = NULL,

    #' @field Data `array` containing the underlying data
    data = NULL,

    # Create a new LPJmLData object
    #    data_array `array` with LPJmL data
    #    meta_data `LPJmLMetaData` Object
    initialize = function(data_array, meta_data = NULL) {
      if (methods::is(meta_data, "LPJmLMetaData") |
          methods::is(meta_data, "NULL")) {
        self$meta <- meta_data
      } else {
        stop("Provide a LPJmLMetaData object for meta data.")
      }
      self$data <- data_array
      if (!is.null(self$meta$variable)) {
        if (self$meta$variable == "grid") {
          private$init_grid()
        }
      }
    },
    #' @description
    #' Method to add a grid to a \link[lpjmlkit](LPJmLData).
    #' See also \link[lpjmlkit](add_grid)
    add_grid = function(...) {
      if (!is.null(self$meta$variable) &&
          self$meta$variable == "grid") {
        stop(paste("not legit for variable", self$meta$variable))
      }
      # check if meta file for grid is located in output location
      grid_file <- list.files(self$meta$data_dir,
                              pattern = "grid.bin.json")
      if (length(grid_file) == 1) {
        # if so get concatenate existing file and data_dir to read grid
        filename <- paste(self$meta$data_dir, grid_file, sep = "/")
        # add support for cell subsets - this is a rough filter since $subset
        #   does not say if cell is subsetted - but ok for now
        if (self$meta$subset_space) {
          self$grid <- read_io(
            filename = filename,
            subset = list(cell = self$dimnames()[["cell"]])
          )
        } else {
          self$grid <- read_io(filename = filename)
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
    # aggregation function, only to be applied for conversions (as_raster, plot)
    #   do not apply to self to not violate data integrity !
    aggregate_array = function(aggregate_list = NULL,
                                ...) {
            data <- self$data
            if (!is.null(aggregate_list)) {
              for (idx in seq_along(aggregate_list)) {
                dim_names <- names(dim(data))
                data <- apply(X = data,
                              MARGIN = dim_names[
                                !dim_names %in% names(aggregate_list)[idx]
                              ],
                              FUN = aggregate_list[[idx]],
                              ...)
              }
            }
            return(data)
    },
    #' @description
    #' Method to get the length of the array of a \link[lpjmlkit](LPJmLData)
    #' object.
    #' See also \link[base](length)
    length = function() {
      return(length(self$data))
    },
    #' @description
    #' Method to get the dimension names and lengths of the array of a
    #' \link[lpjmlkit](LPJmLData) object.
    #' See also \link[base](dim)
    dim = function() {
      dim(self$data)
    },
    #' @description
    #' Method to get the dimensions (list) of the array of a
    #' \link[lpjmlkit](LPJmLData) object.
    #' See also \link[base](dimnames)
    dimnames = function() {
      dimnames(self$data)
    },
    #' @description
    #' Method to get the summary of the array of a
    #' \link[lpjmlkit](LPJmLData) object.
    #' See also \link[base](summary)
    summary = function(dimension = "band",
                       subset = NULL,
                       cutoff = FALSE,
                       ...) {
      data <- subset_array(self$data, subset)
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
          if (!is.null(self$meta$variable) &&
              self$meta$variable == "grid") {

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
        if (!is.null(self$meta$variable) &&
            self$meta$variable == "grid") {
          var_name <- "cell"
          mat_sum <- mat_sum[c(1, 6), ]
        } else {
          var_name <- default(self$meta$variable, "")
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
    #' @description
    #' Method to print the \link[lpjmlkit](LPJmLData).
    #' See also \link[base](print)
    print = function() {
      quotes_option <- options(useFancyQuotes = FALSE)
      on.exit(options(quotes_option))
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      cat(paste0("\u001b[1m", blue_col, "$meta %>%", unset_col, "\n"))
      self$meta$print(all = FALSE, spaces = "  .")
      cat(paste0("\u001b[33;3m",
                 "Note: not printing all meta data, use $meta to get all.",
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
      if (is.null(self$meta$variable) ||
      self$meta$variable != "grid") {
        cat(paste0("\u001b[33;3m",
                   "Note: summary is not weighted by grid area.",
                   unset_col,
                   "\n")
        )
      } else {
        cat(paste0("\u001b[33;3m",
                   ifelse(self$meta$space_format == "cell",
                          "Note: only min & max printed as equivalent to spatial extent.", #nolint
                          "Note: inverted grid (cell as value)! Only min & max printed for sequence of cells."), #nolint
                   unset_col,
                   "\n"))
      }
    }
  ),
  private = list(
    # init grid if variable == "grid"
    init_grid = function() {
      # update grid data
      dimnames(self$data)[["band"]] <- c("lon", "lat")
      # update grid meta data
      self$data <- drop_omit(self$data, omit = "cell")
      self$meta$._init_grid()
      return(invisible(self))
     }
  )
)

# set up method dispatch
#   https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class # nolint
# add additional (important) functions for method dispatch with deep copying
#   x, execute function on copied object and return ("traditional way")

# Function to get the length of the array of a LPJmLData object
length.LPJmLData <- function(x, ...) x$length(...)

# Function to get the dimension names and lengths of the array of a LPJmLData
#   object
dim.LPJmLData <- function(x, ...) x$dim(...)

# Function to get the dimensions (list) of the array of a LPJmLData object
dimnames.LPJmLData <- function(x, ...) x$dimnames(...)

# Function to get the summary of the array of a LPJmLData object.
summary.LPJmLData <- function(x, ...) x$summary(...)


#' Add grid to LPJmLData object
#'
#' Function to add a grid to an \link[lpjmlkit](LPJmLData). The function acts
#' as a \link[lpjmlkit](read_io) function for the grid file and adds it as an
#' `LPJmLData` object itself to the the main object as an attribute `$grid`
#'
#' @param to character vector defining space and/or time format into which
#' corresponding data dimensions should be transformed. Choose from space
#' formats `c("cell", "lon_lat")` and time formats `c("time","year_month_day")`.
#'
#' @return \link[lpjmlkit](LPJmLData) object in selected format
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' }
#'
#' @md
#' @export
add_grid <- function(x, ...) {
  y <- x$clone(deep = TRUE)
  y$add_grid(...)
  return(y)
}

# method dispatch for aggregate_array - only used internally
aggregate_array <- function(x, ...) {
  y <- x$aggregate_array(...)
  return(y)
}
