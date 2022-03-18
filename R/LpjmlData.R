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
  lock_objects = TRUE,
  public = list(
    meta_data = NULL,
    data = NULL,
    # init function
    initialize = function(data_array, meta_data = NULL) {
      if (is(meta_data, "LpjmlMetaData")) {
        self$meta_data <- meta_data
      } else if (is(meta_data, "NULL")) {
        self$meta_data <- NULL
      } else {
        stop("Provide a LpjmlMetaData object for meta_data.")
      }
      self$data <- data_array
    },
    `[` = function(...) {
      self$data[...]
    },
    length = function() {
      length(self$data)
    },
    dim = function() {
      dim(self$data)
    },
    print = function() {
      blue_col <- "\u001b[34m"
      unset_col <- "\u001b[0m"
      meta_fields <- unlist(
        as.list(self$meta_data)[self$meta_data$.__meta_fields_set__]
      )
      if (!is.null(self$meta_data$.__meta_fields_set__)) {
        cat(paste0(blue_col, "$meta_data", unset_col, "\n"))
        cat(
          paste0("  ",
                 blue_col,
                 "$",
                 names(meta_fields),
                 unset_col,
                 " ",
                 meta_fields,
                 collapse = "\n")
        )
        cat("\n")
      }
      data_dims <- lapply(
        dimnames(self$data),
        function(x) {
          if (length(x) > 30) {
            return(append(x[1:4], c("...", tail(x, n = 1))))
          } else {
            return(x)
          }
        }
      )
      cat(paste0(blue_col, "$data", unset_col, "\n"))
      cat(paste0("  ", blue_col, "%>% head", unset_col, "\n"))
      cat("   ", head(self$data), "\n")
      cat(paste0("  ", blue_col, "%>% dim", unset_col, "\n"))
      cat(
        paste0("    ",
               names(dim(self$data)),
               ": ",
               dim(self$data),
               collapse = "\n")
      )
      cat("\n")
      cat(paste0("  ", blue_col, "%>% dimnames", unset_col, "\n"))
      cat(paste0("    ", names(data_dims), ": ", data_dims, collapse = "\n"))
      cat("\n")
    }
  )
)

# set up method dispatch
#   https://stackoverflow.com/questions/50842251/define-a-bracket-operator-on-an-r6-class
`[.LpjmlData`    <- function(obj, ...) obj$`[`(...)
# `[<-.LpjmlData`  <- function(obj, ...) obj$`[<-`(...)
length.LpjmlData <- function(obj, ...) obj$length(...)


# meta_data = read_meta("/p/projects/open/Jannes/lpjml/testing/meta/runs/output/lu/aconv_loss_evap.bin.json")
# data_array <- array(1, dim=c(cells=67420,months=12,bands=12), dimnames=list(cells=1:67420,months=1:12,years=2001:2012))
# oo = LpjmlData$new(data_array, meta_data)