#' Length of an LPJmLData data array
#'
#' Function to get the length of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#'@return A non-negative integer or numeric (which will be rounded down).
#'
#' @md
#' @export
length.LPJmLData <- function(x) x$length()

# Length of the array of an LPJmLData object
LPJmLData$set("private",
              ".length",
              function() {
    length(self$data)
  }
)


#' Dimensions of an LPJmLData data array
#'
#' Function to get the dimensions of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#' @return For the default method, either `NULL` or a numeric vector, which is
#' coerced to integer (by truncation).
#'
#' @md
#' @export
dim.LPJmLData <- function(x) x$dim()

# Dimensions of the data array of an LPJmLData object
LPJmLData$set("private",
              ".dim",
              function() {
    dim(self$data)
  }
)


#' Dimnames of an LPJmLData data array
#'
#' Function to get the dimnames (list) of the data array of an LPJmLData object.
#'
#' @param x [LPJmLData] object
#'
#' @return A list of the same length as dim(x). Components are character vectors
#' with positive length of the respective dimension of x.
#'
#' @md
#' @export
dimnames.LPJmLData <- function(x) x$dimnames()

# dimnames (list) of the data array of an LPJmLData object
LPJmLData$set("private",
              ".dimnames",
              function() {
    dimnames(self$data)
  }
)


#' LPJmLData object summary
#'
#' Function to get the summary of the data array of an LPJmLData object.
#' See also \link[base]{summary}.
#'
#' @param object [LPJmLData] object
#'
#' @param ... Further arguments:
#' * `dimension` for which a summary is printed for every element
#'   (in style of matrix summary). Default is `dimension = "band"`. Choose from
#'   available dimensions like `"time"` or `"cell"`.
#' * `subset` list of array dimension(s) as name/key and corresponding subset
#'   vector as value, e.g. `list(cell = c(27411:27415)`. More information at
#'   [`subset.LPJmLData()`].
#' * `cutoff` (logical) If `TRUE` summary for dimension elements > 16 are
#'   cut off.
#' * Additional arguments to be passed on to \link[base]{summary}.
#'
#' @return Summary for object of class matrix (see \link[base]{summary}) for
#' selected dimension(s) and if defined subset.
#'
#' @md
#' @export
summary.LPJmLData <- function(object,
                              ...) {
  object$summary(...)
}


# Summary of the data array of an LPJmLData object
LPJmLData$set("private",
              ".summary",
              function(dimension = "band",
                       subset = NULL,
                       cutoff = FALSE,
                       ...) {

    data <- subset_array(self$data, subset, drop = FALSE)

    if (dimension %in% names(dimnames(data)) &&
        length(which(dim(data) > 1)) > 1) {

      mat_sum <- data %>%
        apply(dimension, c)

      if (dim(mat_sum)[2] > 16 && cutoff) {
        message(
          col_note(
            paste0(
              "Note: not printing all ",
              dimension,
              "s summary, use $summary() or summary() to get all."
            )
          )
        )

        mat_sum[, seq_len(16)] %>%
          summary(...)

      } else {

        mat_sum %>%
          summary(...)
      }

    } else {
      # Check if dimension has length > 1 then rbind vector data to get
      # summary for each dimension name
      if (length(dimnames(data)[[dimension]]) > 1) {
        mat_sum <- summary(rbind(data), ...)
      } else {
        mat_sum <- summary(matrix(data), ...)
      }
      var_name <- dimnames(data)[[dimension]]

      # Assign dimname(s) as name for (each) summary
      space_len <- pmax((9 - nchar(var_name)) * 0.5, 0)
      attr(mat_sum, "dimnames")[[2]] <- paste0(
        sapply(space_len, function(x) paste0(rep(" ", x), collapse = "")), # nolint:undesirable_function_linter.
        var_name
      )
      return(mat_sum)
    }
  }
)
