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
#' @param force_idx logical. If TRUE indices are always used, when numerical
#' values are provided, even when subsetting with years or lon, lat
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
#'                           subset_list = list(band=c("band1", "band3")))
#' dimnames(my_subset)[3]
#' # $ band
#' #   [1] "band1"
#' #   [2] "band3"
#'
#' # replace subset
#' my_replacement <- replace_array(my_subset,
#'                                 subset_list = list(band=c("band1")),
#'                                 0)
#' @aliases asub replace_array
#' @export
subset_array <- function(x, subset_list = NULL, drop=TRUE, force_idx = FALSE) {
  if (is.null(subset_list)) {
    return(x)
  }
  if (drop) {
    argum <- c(alist(x),
               subarray_argument(x, subset_list, force_idx))
  } else {
    argum <- c(alist(x),
               subarray_argument(x, subset_list, force_idx),
               drop = FALSE)
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
subarray_argument <- function(x, subset_list, force_idx = FALSE) {
  # DRY
  dim_names <- names(dimnames(x))
  subset_names <- names(subset_list)

  # check matching of subset names and dim_names
  match_x <- which(dim_names %in% subset_names)
  match_subset <- stats::na.omit(match(dim_names, subset_names))

  # check for non matching dimensions
  valids <- subset_names %in% dim_names
  if (!all(valids)) {
    non_valids <- which(!valids)
    stop(
      paste0(
        ifelse(length(non_valids) > 1, "Dimension names ", "Dimension name "),
        "\u001b[34m",
        paste0(subset_names[non_valids], collapse = ", "),
        "\u001b[0m",
        ifelse(length(non_valids) > 1, " are ", " is "),
        "not valid. Please choose from available dimension names ",
        "\u001b[34m",
        paste0(dim_names, collapse = ", "),
        "\u001b[0m."
      ),
      call. = FALSE
    )
  }
  subset_list <- mapply(
    function(x, y, dim_name) {
      # for lon, lat calculate nearest neighbor for each provided value if not
      #   character
      if (!is.character(x) && dim_name %in% c("lon", "lat") && !force_idx) {
        return(
          sapply(x, function(x, y) which.min(abs(as.numeric(y) - x)), y) %>%
            unique()
        )
      }
      # subsetting with character strings (directly dimnames)
      if (is.character(x)) {
        return(which(tolower(y) %in% tolower(x)))
      } else {
        # exception for dimension year, use numeric years quasi as character
        #   string
        if (dim_name == "year" && !force_idx) {
          return(which(y %in% as.character(x)))
        }
        return(x)
      }
    },
    subset_list[match_subset],
    dimnames(x)[match_x],
    dim_names[match_x],
    SIMPLIFY = FALSE
  )
  argument <- rep(list(bquote()), length(dim(x)))
  # insert the wanted dimension slices
  argument[match_x] <- subset_list
 return(argument)
}


subset_array_pair <- function(x,
                              pair = NULL) {
  # get pair in the same order as dimensions
  pair <- pair[stats::na.omit(match(names(dimnames(x)), names(pair)))]
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
    array(dim = dim(x), dimnames = dimnames(x)) %>%
    subset_array(as.list(pair), drop = FALSE)

  y <- subset_array(x, as.list(pair), drop = FALSE)
    # get dim & dimnames of dimensions not matching pair
  y[is.na(subset_mask)] <- NA

  return(y)
}

# drop 1 dimensional dimension except those that are selected by name
drop_omit <- function(x, omit_dim) {
  dims <- dim(x)
  dims_check <- dims == 1 & !(names(dims) %in% omit_dim)
  return(abind::adrop(x, dims_check))
}
