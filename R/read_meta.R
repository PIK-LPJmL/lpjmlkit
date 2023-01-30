#' Read an LPJmL meta or header file
#'
#' Reads a meta JSON file or an LPJmL input or output that includes a file
#' header.
#'
#' @param filename character string representing path
#' (if differs from current working directory) and filename
#'
#' @param ... further arguments passed to [`read_header`] if header file is read
#'
#' @return LPJmLMetaData object
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
#' @md
#' @export
read_meta <- function(filename, ...) {
  # get and provide data path for lazy data purposes (e.g. load grid later)
  pathname <- dirname(filename)

  # detect LPJmL file types - "meta", "clm" or other
  file_type <- detect_type(filename)

  # meta (JSON) file handling
  if (file_type == "meta") {
    meta_object <- jsonlite::read_json(path = filename, simplify = TRUE) %>%
      LPJmLMetaData$new(data_dir = pathname)

  # input output data containing a header handling
  } else if (file_type == "clm") {
    meta_object <- read_header(filename, ...) %>%
      LPJmLMetaData$new(data_dir = pathname)

  # other formats are not supported yet
  } else {
    stop("Non readable (meta) file format.")
  }

  return(meta_object)
}