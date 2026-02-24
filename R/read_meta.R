#' Read an LPJmL meta file, NetCDF or binary file header
#'
#' Reads the meta information of an LPJmL Meta (JSON) file, a NetCDF file or
#' the header of a binary LPJmL input or output file. For NetCDF files, an
#' additional arguments can be passed to the internal [`read_cdf_meta`]
#' function to specify the variable name if multiple variables are present
#' in the file and automatic detection fails.
#'
#' @param filename Character string representing path
#'   (if different from current working directory) and filename.
#'
#' @param ... Additional arguments passed to [`read_header`] if header file is
#'   read.
#'
#' @return An [`LPJmLMetaData`] object.
#'
#' @examples
#' \dontrun{
#' meta <- read_meta(filename = "mpft_npp.bin.json")
#'
#'  meta$sim_name
#'  # [1] "LPJmL Run"
#'
#'  meta$firstcell
#'  # [1] 27410
#'
#'  meta$band_names[1]
#'  # [1] "tropical broadleaved evergreen tree"
#' }
#'
#' @md
#' @export
read_meta <- function(filename, ...) {
  # Get and provide data path for lazy data purposes (e.g. load grid later)
  pathname <- dirname(filename)

  # Detect LPJmL file types - "meta", "clm" or other
  file_type <- detect_io_type(filename)

  # Meta (JSON) file handling
  if (file_type == "meta") {
    meta_object <- jsonlite::read_json(path = filename, simplifyVector = TRUE)
    if (meta_object$format == "cdf" && meta_object$name == "grid") {
      meta_object$variable <- "cellid"
    }
    meta_object <- meta_object %>% LPJmLMetaData$new(data_dir = pathname) # nolint:pipe_consistency_linter
    # Handling of input or output file containing a header
  } else if (file_type == "clm") {
    header <- read_header(filename, ...)
    additional_attributes <- list(
      format = unname(file_type),
      offset = unname(get_headersize(header))
    )

    meta_object <- LPJmLMetaData$new(
      header,
      additional_attributes = additional_attributes,
      data_dir = pathname
    )

    # Handling of CDF files
  } else if (file_type == "cdf") {
    meta_data_list <- read_cdf_meta(filename,
                                    variable_name = NULL,
                                    silent = TRUE)

    meta_object <- LPJmLMetaData$new(
      x = meta_data_list,
      data_dir = dirname(filename)
    )

    # Other formats are not supported yet
  } else {
    stop("Non readable (meta) file format.")
  }

  meta_object
}
