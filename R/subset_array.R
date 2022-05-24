#' Subset a named array
#'
#' Subset an array with supplied dimnames and - if defined - replace it.
#'
#' @param x array with named dimensions
#'
#' @param subset_list names list with names being names of dimnames to be
#' subsetted. list values can either be numeric (leads to subsetting by indices)
#' or character strings (leads to subsetting by dimnames). Default is `list()`
#'
#' @param drop logical. If TRUE (default), dimensions are dropped when dimension
#' has length == 1, else dimension is kept.
#'
#' @param y array/vector of the same dimension as referred to by the subset_list
#'
#' @return array or vector (if `drop=TRUE` and one only dimension left)
#'
#' @usage
#' subset_array(x, subset_list = NULL, drop = TRUE)
#' asub(x, subset_list = NULL, drop = TRUE)
#' replace_array(x, subset_list, y)
#'
#' @examples
#' my_array <- array(1,
#'                   dim=c(cell=67, month=12, band=3),
#'                   dimnames=list(cell=0:66,
#'                                 month=1:12
#'                                 band=c("band1", "band2", "band3")))
#' my_subset <- subset_array(my_array,
#'                           subset_list(bands=c("band1", "band3")))
#' dimnames(my_subset)[3]
#' # $ band
#' #   [1] "band1"
#' #   [2] "band3"
#'
#' # replace subset
#' my_replacement <- replace_array(my_subset,
#'                                 subset_list(bands=c("band1")),
#'                                 0)
#' @aliases asub replace_array
#' @export
subset_array <- function(x, subset_list = NULL, drop=TRUE) {
  if (is.null(subset_list)) {
    return(x)
  }
  if (drop) {
    argum <- c(alist(x), subarray_argument(x, subset_list))
  } else {
    argum <- c(alist(x), subarray_argument(x, subset_list), drop = FALSE)
  }
  do.call("[", argum) %>%
    return()
}


#' @export
asub <- subset_array


#' @export
replace_array <- function(x, subset_list, y) {
  argum <- c(alist(x), subarray_argument(x, subset_list), alist(y))
  do.call("[<-", argum) %>%
    return
}


# https://stackoverflow.com/questions/47790061/r-replacing-a-sub-array-dynamically
subarray_argument <- function(x, subset_list) {
  # first a suitable empty list
  match_x <- which(names(dimnames(x)) %in% names(subset_list))
  match_subset <- stats::na.omit(match(names(dimnames(x)), names(subset_list)))
  subset_list <- mapply(
    function(x, y, dim_name) {
      if (is.character(x)) {
        return(which(y %in% x))
      } else {
        if (dim_name == "year") {
          return(which(y %in% as.character(x)))
        }
        return(x)
      }
    },
    subset_list[match_subset],
    dimnames(x)[match_x],
    names(dimnames(x))[match_x],
    SIMPLIFY = FALSE
  )
  argument <- rep(list(bquote()), length(dim(x)))
  # insert the wanted dimension slices
  argument[match_x] <- subset_list
 return(argument)
}


subset_array_pair <- function(x,
                              subset_pair = NULL) {
  pair_dims <- names(subset_pair)

  idim1 <- match(subset_pair[[1]],
                 as.numeric(dimnames(x)[[pair_dims[1]]]))
  idim2 <- match(subset_pair[[2]],
               as.numeric(dimnames(x)[[pair_dims[2]]]))

  pre_mask <- array(NA,
                    dim = dim(x)[pair_dims],
                    dimnames = dimnames(x)[pair_dims])
  idims <- cbind(idim1, idim2) %>%
    `colnames<-`(pair_dims)
  pre_mask[idims] <- 1

  subset_mask <- array(pre_mask, dim(x), dimnames = dimnames(x))

  other_dimnames <- dimnames(x)[which(names(dimnames(x)) != pair_dims)]
  other_dims <- lapply(other_dimnames, length)
  mask_dims <- lapply(subset_pair, length)
  y <- array(NA,
             dim = c(mask_dims, other_dims),
             dimnames = do.call(list,
                             args = c(subset_pair,
                                      other_dimnames)))

  y[] <- x[!is.na(subset_mask)]

  return(y)
}