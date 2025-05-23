#' Read an LPJmL meta file or binary file header
#'
#' Reads a meta JSON file or the header of a binary LPJmL input or output file.
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
#'  meta <- read_meta(filename = "mpft_npp.bin.json")
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
    meta_object <- jsonlite::read_json(path = filename, simplifyVector = TRUE) %>%
      LPJmLMetaData$new(data_dir = pathname)

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

    # Other formats are not supported yet
  } else {
    stop("Non readable (meta) file format.")
  }

  meta_object
}
