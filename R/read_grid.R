#' @title Read LPJmL input and output grid files
#'
#' @description Generic function to read LPJmL input & output files in different
#'   formats. Depending on the format, arguments can be automatically detected
#'   or have to be passed as individual arguments.
#'
#' @param ... See [read_io] for further arguments.
#' @return An [LPJmLGridData] object.
#' @examples
#' \dontrun{
#' my_grid <- read_io("grid.bin.json")
#'
#' }
#' @details
#' See [read_io] for more details.
#' @export
read_grid <- function(...) {
  read_io(...) %>%
    LPJmLGridData$new()
}