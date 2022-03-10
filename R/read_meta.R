#' Read a LPJmL meta file
#'
#' Reads a meta JSON file.
#'
#' @param filename character string representing path
#' (if differs from current working directory) and filename
#'
#' @return (nested) list object
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
    meta_list <- jsonlite::read_json(path = filename, simplify = FALSE)
  return(meta_list)
}
