#' Read a LPJmL meta file
#'
#' Reads a meta JSON file.
#'
#' @param filename character string representing path
#' (if differs from current working directory) and filename
#'
#' @return LPJmLMetaData object
#'
#' @examples
#' \dontrun{
#'  meta <- read_meta(filename = "mpft_npp.bin.json")
#'
#'  meta[[sim_name]]
#'  # [1] "LPJmL Run"
#'
#'  meta[[firstcell]]
#'  # [1] 27410
#'
#'  meta[[pft]][[1]]
#'  # [1] "tropical broadleaved evergreen tree"
#' }
#' @export
read_meta <- function(filename) {
    # get and provide data path for lazy data purposes (e.g. load grid later)
    pathname <- dirname(filename)
    meta_object <- jsonlite::read_json(path = filename, simplify = TRUE) %>%
      LPJmLMetaData$new(data_dir = pathname)
  return(meta_object)
}