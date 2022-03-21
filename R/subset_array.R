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

replace_array <- function(x, subset_list, y) {
  argum <- c(alist(x), subarray_argument(x, subset_list), alist(y))
  do.call("[<-", argum) %>%
    return
}

# https://stackoverflow.com/questions/47790061/r-replacing-a-sub-array-dynamically
subarray_argument <- function(x, subset_list) {
  # first a suitable empty list
  match_x <- which(names(dimnames(x)) %in% names(subset_list))
  match_subset <- na.omit(match(names(dimnames(x)), names(subset_list)))
  subset_list <- mapply(
    function(x, y) {
      if (is.character(x)) {
        return(which(y %in% x))
      } else {
        return(x)
      }
    },
    subset_list[match_subset],
    dimnames(x)[match_x],
    SIMPLIFY = FALSE
  )
  argument <- rep(list(bquote()), length(dim(x)))
  # insert the wanted dimension slices
  argument[match_x] <- subset_list
 return(argument)
}
