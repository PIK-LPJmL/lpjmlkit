#' Length of an LPJmLData data array
#'
#' Function to get the length of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#' @md
#' @export
length.LPJmLData <- function(x) x$length()

# length of the array of an LPJmLData
LPJmLData$set("private",
              ".length",
              function() {
    return(length(self$data))
  }
)


#' Dimensions of an LPJmLData data array
# '
#' Function to get the dimensions of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#' @md
#' @export
dim.LPJmLData <- function(x) x$dim()

# dimension names and lengths of the array of an LPJmLData object.
LPJmLData$set("private",
              ".dim",
              function() {
    dim(self$data)
  }
)


#' Dimnames of an LPJmLData data array
# '
#' Function to get the dimnames (list) of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#' @param ... Further arguments to be passed on to \link[base]{dimnames}
#'
#' @md
#' @export
dimnames.LPJmLData <- function(x, ...) x$dimnames(...)

# dimensions (list) of the array of an LPJmLData object.
LPJmLData$set("private",
              ".dimnames",
              function(...) {
    dimnames(self$data, ...)
  }
)


#' LPJmLData object summary
# '
#' Function to get the summary of the data array of an LPJmLData object.
#' See also \link[base]{summary}
#'
#' @param object [LPJmLData] object
#'
#' @param ... Further arguments:
#' * `dimension` for which a summary is printed for every element
#' (in style of matrix summary). Default is `dimension = band"`. Choose from
#' available dimensions like `"time"` or `"cell"`.
#' * `subset` list of array dimension(s) as name/key and corresponding subset
#' vector as value, e.g. `list(cell = c(27411:27415)`. More information at
#' [`subset.LPJmLData`].
#' * `cutoff` (logical) If `TRUE` summary for dimension elements > 16 are
#' cut off.
#' * Additional arguments to be passed on to \link[base]{summary}
#'
#' @md
#' @export
summary.LPJmLData <- function(object,
                              ...) {
  object$summary(...)
}


# summary of the array of the array of an LPJmLData object.
LPJmLData$set("private",
              ".summary",
              function(dimension = "band",
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
        if (!is.null(private$.meta$variable) &&
            private$.meta$variable == "grid") {

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

      if (!is.null(private$.meta$variable) &&
          private$.meta$variable == "grid") {
        var_name <- "cell"
        mat_sum <- mat_sum[c(1, 6), ]

      } else {
        var_name <- default(private$.meta$variable, "")
      }

      space_len <- ifelse(nchar(var_name) > 8,
                          0,
                          4 - sqrt(nchar(var_name)))

      paste0(c(rep(" ", space_len), var_name, "\n")) %>%
        append(paste0(mat_sum, collapse = "\n ")) %>%
      cat()
      return(bquote())
    }
  }
)
