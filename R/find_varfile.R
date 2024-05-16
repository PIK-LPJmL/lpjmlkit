#' Search for a variable file in a directory
#'
#' Function to search for a file containing a specific variable in a specific
#' directory.
#'
#' @param searchdir Directory where to look for the variable file.
#' @param variable Single character string containing the variable to search for
#' @param strict Boolean. If set to `TRUE`, file must be named "variable.**",
#'   where "**" is one or two file extensions with 3 or 4 characters, e.g.
#'   "grid.bin.json" if `variable = "grid"`. If set to `FALSE`, the function
#'   will first try to match the strict pattern. If unsuccessful, any filename
#'   that starts with "variable" will be matched.
#' @return Character string with the file name of a matched file, including the
#'   full path.
#'
#' @details This function looks for file names in `searchdir` that match the
#'   `pattern` parameter in its [`list.files()`] call. Files of type "meta" are
#'   preferred. Files of type "clm" are also accepted. The function returns an
#'   error if no suitable file or multiple files are found.
#' @export
find_varfile <- function(searchdir, variable = "grid", strict = FALSE) {
  if (length(variable) != 1 || !is.character(variable)) {
    stop(col_var("variable"), " must be a single character string")
  }
  # This will only match file names "variable.*", where * is one or two file
  # extensions with 3 or 4 characters, e.g. "grid.bin" or "grid.bin.json".
  var_files <- list.files(
    path = searchdir,
    pattern = paste0("^", variable, "(\\.[[:alpha:]]{3,4})+$"),
    full.names = TRUE
  )
  if (length(var_files) > 0) {
    var_types <- sapply(var_files, detect_io_type) # nolint:undesirable_function_linter.
    # Prefer "meta" file_type if present
    if (length(which(var_types == "meta")) == 1) {
      filename <- var_files[match("meta", var_types)]
    } else if (length(which(var_types == "clm")) == 1) {
      # Second priority "clm" file_type
      filename <- var_files[match("clm", var_types)]
    } else if (strict) {
      # Stop if either multiple files per file type or not the right type have
      # been detected
      stop(
        "Cannot detect ", col_var(variable), " file automatically."
      )
    }
  } else if (strict) {
    # Stop if no file name matching pattern detected
    stop(
      "Cannot detect ", col_var(variable), " file automatically."
    )
  } else {
    # Less strict pattern matching any file name that starts with "grid*".
    var_files <- list.files(
      path = searchdir,
      pattern = paste0("^", variable),
      full.names = TRUE
    )
    if (length(var_files) > 0) {
      var_types <- sapply(var_files, detect_io_type) # nolint:undesirable_function_linter.
      # Prefer "meta" file_type if present
      if (length(which(var_types == "meta")) == 1) {
        filename <- var_files[match("meta", var_types)]
      } else if (length(which(var_types == "clm")) == 1) {
        # Second priority "clm" file_type
        filename <- var_files[match("clm", var_types)]
      } else {
        # Stop if either multiple files per file type or not the right type have
        # been detected
        stop(
          "Cannot detect ", col_var(variable), " file automatically."
        )
      }
    } else {
      # Stop if no file name matching pattern detected
      stop(
        "Cannot detect ", col_var(variable), " file automatically."
      )
    }
  }
  filename
}
