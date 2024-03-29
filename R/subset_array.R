#' Subset a named array
#'
#' Subset an array with the supplied dimnames and - if defined - replace values.
#'
#' @param x An array with named dimensions.
#'
#' @param ... One or several vectors of indices or character strings to be used
#'   to subset `x`. Argument names refer to the dimension name to be subset,
#'   while argument values specify the selected elements along the respective
#'   dimension. Examples: `cell = c(27411:27416)`, `band = -c(14:16, 19:32)`,
#'   `band = c("rainfed rice","rainfed maize")`.
#'
#' @param drop Logical. If `TRUE` (default), dimensions with a length of 1 are
#'   dropped from the result. Otherwise, they are kept.
#'
#' @return array (or vector if `drop = TRUE` and only one dimension is left)
#'   of the selected subset of `x`.
#'
#' @examples
#' my_array <- array(1,
#'                   dim = c(cell = 67, month = 12, band = 3),
#'                   dimnames = list(cell = 0:66,
#'                                 month = 1:12,
#'                                 band = c("band1", "band2", "band3")))
#' my_subset <- asub(my_array,
#'                   band = c("band1", "band3"))
#' dimnames(my_subset)[3]
#' # $ band
#' #   [1] "band1"
#' #   [2] "band3"
#'
#' @export
asub <- function(x,
                 ...,
                 drop = TRUE) {
  x %>%
    subset_array(subset_list = list(...),
                 drop = drop) %>%
    return()
}


# value Array/vector of replacement values. Note: If `value` does not
# have the same dimensions as selected by (`...`), automatic replication
# is done by **R** to extend `value` to the required length.
# If `value` is specified, returns an array with the same dimensions as
# `x` where values selected by `(...)` are replaced by values from `value`.
`asub<-` <- function(x, ..., value) {
  argum <- c(alist(x),
             subarray_argument(x, list(...)), alist(value))
  do.call("[<-", argum) %>%
    return()
}


subset_array <- function(x,
                         subset_list = NULL,
                         drop = TRUE,
                         silent = FALSE) {

  if (length(subset_list) == 0) {
    return(x)
  }

  # Filter for NAs in subset_list
  if (length(subset_list) > 0 && any(sapply(subset_list, anyNA))) { # nolint:undesirable_function_linter.
    if (!silent) {
      warning(
        "Removing NA values from ",
        paste(
          "subset_list[[", dQuote(names(which(sapply(subset_list, anyNA)))), # nolint:undesirable_function_linter.
          "]]",
          sep = "", collapse = ", "
        )
      )
    }

    subset_list <- lapply(subset_list, stats::na.omit)

    # Remove empty subsets
    before <- names(subset_list)
    subset_list <- subset_list[which(sapply(subset_list, length) > 0)] # nolint:undesirable_function_linter.

    if (length(subset_list) < length(before) && !silent) {
      warning(
        paste(
          "subset_list[[", dQuote(setdiff(before, names(subset_list))), "]]",
          sep = "", collapse = ", "
        ),
        " empty after removal of NAs. ",
        ifelse(
          length(before) - length(subset_list) > 1, "Dimensions", "Dimension"
        ),
        " not subsetted."
      )
    }
  } else if (length(subset_list) > 0 && any(sapply(subset_list, length) == 0)) { # nolint:undesirable_function_linter

    # Remove empty subsets
    before <- names(subset_list)
    subset_list <- subset_list[which(sapply(subset_list, length) > 0)] # nolint:undesirable_function_linter.

    if (length(subset_list) < length(before) && !silent) {
      warning(
        paste(
          "subset_list[[", dQuote(setdiff(before, names(subset_list))), "]]",
          sep = "", collapse = ", "
        ),
        " empty. ",
        ifelse(
          length(before) - length(subset_list) > 1, "Dimensions", "Dimension"
        ),
        " not subsetted."
      )
    }
  }

  if (drop) {
    argum <- c(alist(x),
               subarray_argument(x, subset_list))

  } else {
    argum <- c(alist(x),
               subarray_argument(x, subset_list),
               drop = FALSE)
  }

  do.call("[", argum) %>%
    return()
}


# https://stackoverflow.com/questions/47790061/r-replacing-a-sub-array-dynamically # nolint
subarray_argument <- function(x, subset_list) {
  # DRY
  dim_names <- names(dimnames(x))
  subset_names <- names(subset_list)

  # Check matching of subset names and dim_names
  match_x <- which(dim_names %in% subset_names)
  match_subset <- stats::na.omit(match(dim_names, subset_names))

  # Check for non matching dimensions
  valids <- subset_names %in% dim_names

  if (!all(valids)) {
    nonvalids <- which(!valids)
    stop(
      ifelse(length(nonvalids) > 1, "Dimension names ", "Dimension name "),
      paste0(col_var(subset_names[nonvalids]), collapse = ", "),
      ifelse(length(nonvalids) > 1, " are ", " is "),
      "not valid. Please choose from available dimension names ",
      paste0(col_var(dim_names), collapse = ", "),
      call. = FALSE
    )
  }

  subset_list <- mapply( # nolint:undesirable_function_linter.

    function(x, y, dim_name) {
      # For lon, lat calculate nearest neighbor for each provided value if not
      #   character.

      if (dim_name %in% c("lon", "lat") && is.character(x)) {
        return(sapply(x, # nolint:undesirable_function_linter.
                      function(x, y) {
                        which.min(abs(as.numeric(y) - as.numeric(x)))
                      },
                      y) %>% unique())
      }

      # Subsetting with character strings (directly dimnames)
      if (is.character(x)) {
        # Get valid subsets - non valids are NA
        valid_sub <- match(tolower(x), tolower(y))

        # Check if it contains NA - if so stop and print non valid subsets
        check_string_index(x, valid_sub, dim_name)
        return(valid_sub)

      } else {
        # Check if indices are valid - if not stop and print non valid indices
        check_index(x, y, dim_name)

        return(x)
      }
    },

    subset_list[match_subset],
    dimnames(x)[match_x],
    dim_names[match_x],
    SIMPLIFY = FALSE
  )

  argument <- rep(list(bquote()), length(dim(x)))

  # Insert the wanted dimension slices
  argument[match_x] <- subset_list

  argument
}


subset_array_pair <- function(x,
                              pair = NULL) {
  # Get pair in the same order as dimensions
  pair <- pair[stats::na.omit(match(names(dimnames(x)), names(pair)))]
  pair_names <- names(pair)

  # Ensure that coordinates are character vectors.
  if (length(pair) == 0 ||
        !all(sapply(pair, is.character) | sapply(pair, is.factor), na.rm = TRUE)) { # nolint
    stop("Values for coordinate pairs must be supplied as strings")
  }

  # Look up/match vector of dimnames of x (for performance in loop)
  idim1_table <- as.numeric(dimnames(x)[[pair_names[1]]])

  # Match first dim of pair against dimnames of x
  idim1 <- match(as.numeric(pair[[1]]),
                 idim1_table)

  # For NAs (non matches) find nearest dimname values
  for (isna in which(is.na(idim1))) {
    idim1[isna] <- which.min(abs(idim1_table - as.numeric(pair[[1]])[isna]))
  }

  # Same for second dim of pair and so forth.
  idim2_table <- as.numeric(dimnames(x)[[pair_names[2]]])
  idim2 <- match(as.numeric(pair[[2]]),
                 idim2_table)

  for (isna in which(is.na(idim2))) {
    idim2[isna] <- which.min(abs(idim2_table - as.numeric(pair[[2]])[isna]))
  }

  # Index vectors to 2 column matrix for pair subsetting
  idims <- cbind(idim1, idim2) %>%
    `colnames<-`(pair_names)

  # Create mask from dimension name pair
  if (match(pair_names[1], names(dim(x))) > 1) {
    dupl <- prod(dim(x)[seq_len(match(pair_names[1], names(dim(x))) - 1)])
  } else {
    dupl <- 1
  }
  subset_mask <- array(NA,
                       dim = dim(x)[pair_names],
                       dimnames = dimnames(x)[pair_names]) %>%
    `[<-`(idims, 1) %>%
    rep(each = dupl) %>%
    array(dim = dim(x), dimnames = dimnames(x)) %>%
    subset_array(as.list(pair), drop = FALSE)

  y <- subset_array(x, as.list(pair), drop = FALSE)

  # Get dim & dimnames of dimensions not matching pair
  y[is.na(subset_mask)] <- NA

  y
}


# Check if indices exist in dimension of array
check_index <- function(x, y, dim_name) {
  if (any(abs(x) > length(y) | x == 0)) {
    nonvalids <- which(abs(x) > length(y) | x == 0)
    stop_subset(x, nonvalids, dim_name)
  }
}


# Check if character vector elements exist in dimension of array
check_string_index <- function(x, valids, dim_name) {
  if (any(is.na(valids))) {
    nonvalids <- which(is.na(valids))
    stop_subset(x, nonvalids, dim_name, TRUE)
  }
}


# Print error for non valid elements of dimension
stop_subset <- function(x, nonvalids, dim_name, string_index = FALSE) {
  if (string_index) {
    x_nonvalid <- paste0(dQuote(x[nonvalids]), collapse = ", ")
  } else {
    x_nonvalid <- paste0(x[nonvalids], collapse = ", ")
  }

  stop(
    "For dimension ",
    col_var(dim_name),
    ifelse(string_index, " string", ""),
    ifelse(length(nonvalids) > 1, " indices ", " index "),
    col_var(x_nonvalid),
    ifelse(length(nonvalids) > 1, " are", " is"),
    " not valid.",
    call. = FALSE
  )
}
