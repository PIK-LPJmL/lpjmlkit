# Function to rewrite params in terms of JSON keys of precompiled
#   and read (as list) lpjml.js > config.json
#   Nested keys can be reached via "key.subkey,subsubkey" -> "input.soil.name".
#   For the second level (only) indices can be used, lists occur quite on that
#   level, e.g. "key.1.subkey" -> "soilpar.1.name"
mutateConfigParam <- function(x, params, exclude_macros) {
  # every column represents a key in config.json
  params[c("order", "dependency", exclude_macros)] <- NULL

  for (colname in colnames(params)) {
    # test if NA is supplied, then default value is used
    param_value <- unlist(params[[colname]])
    if (is.na(param_value)) next

    # split keys for each level
    keys <- strsplit(colname, "[.]")[[1]]

    # test for length and digits (indices) -> handle each case
    if (length(keys) > 1) {
      if (grepl("^[[:digit:]]+$", keys)[2]) {
        if (length(keys) > 2) {
          if (is.null(x[[keys[1]]][[
            as.integer(keys[2])
          ]][[keys[3:length(keys)]]])) {
            stop(paste(colname, "is not legit!"))
          } else {
            x[[keys[1]]][[as.integer(keys[2])]][[
              keys[3:length(keys)]
            ]] <- param_value
          }
        } else {
          if (is.null(x[[keys[1]]][[as.integer(keys[2])]])) {
            stop(paste(colname, "is not legit!"))
          } else {
            x[[keys[1]]][[as.integer(keys[2])]] <- param_value
          }
        }
      } else {
        if (is.null(x[[keys]])) {
          stop(paste(colname, "is not legit!"))
        } else {
          x[[keys]] <- param_value
        }
      }
    } else {
      if (is.null(x[[keys]])) {
        stop(paste(colname, "is not legit!"))
      } else {
        x[[keys]] <- param_value
      }
    }
  }
  return(x)
}