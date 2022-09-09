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
#'                                 month=1:12,
#'                                 band=c("band1", "band2", "band3")))
#' my_subset <- subset_array(my_array,
#'                           subset_list = list(bands=c("band1", "band3")))
#' dimnames(my_subset)[3]
#' # $ band
#' #   [1] "band1"
#' #   [2] "band3"
#'
#' # replace subset
#' my_replacement <- replace_array(my_subset,
#'                                 subset_list = list(bands=c("band1")),
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
    return()
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
                              pair = NULL) {
  pair_names <- names(pair)

  # look up/match vector of dimnames of x (for performance in loop)
  idim1_table <- as.numeric(dimnames(x)[[pair_names[1]]])
  # match first dim of pair against dimnames of x
  idim1 <- match(as.numeric(pair[[1]]),
                 idim1_table)
  # for NAs (non matches) find nearest dimname values
  #   - the more the longer the iteration
  for (isna in which(is.na(idim1))) {
    idim1[isna] <- which.min(abs(idim1_table - as.numeric(pair[[1]])[isna]))
  }

  # same for second dim of pair ...
  idim2_table <- as.numeric(dimnames(x)[[pair_names[2]]])
  idim2 <- match(as.numeric(pair[[2]]),
                 idim2_table)
  for (isna in which(is.na(idim2))) {
    idim2[isna] <- which.min(abs(idim2_table - as.numeric(pair[[2]])[isna]))
  }

  # index vectors to 2 column matrix for pair subsetting
  idims <- cbind(idim1, idim2) %>%
    `colnames<-`(pair_names)

  # create mask from dimension name pair
  subset_mask <- array(NA,
                    dim = dim(x)[pair_names],
                    dimnames = dimnames(x)[pair_names]) %>%
    `[<-`(idims, 1) %>%
    array(dim(x), dimnames = dimnames(x))

  # get dim & dimnames of dimensions not matching pair
  other_dimnames <- dimnames(x)[which(names(dimnames(x)) != pair_names)]
  # dim() workaround
  other_dims <- lapply(other_dimnames, length)
  mask_dims <- lapply(pair, length)
  y <- array(NA,
             dim = c(mask_dims, other_dims),
             dimnames = do.call(list,
                             args = c(pair,
                                      other_dimnames)))

  y[] <- x[!is.na(subset_mask)]

  return(y)
}

# drop 1 dimensional dimension except those that are selected by name
drop_omit <- function(x, omit_dim) {
  dims <- dim(x)
  dims_check <- dims == 1 & !(names(dims) %in% omit_dim)
  return(abind::adrop(x, dims_check))
}

# pair=tibble(lat=c(-55.75,-55.25,-54.5), lon=c(-179.75, -179.25, -178.9))
