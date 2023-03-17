# subset method roxygen documentation in LPJmlData.R
LPJmLGridData$set("private",
                  ".subset_space",
                  function(subset_list) {

    # Only space dimensions valid here
    subset_list <- select_space_dims(subset_list)

    self$.__set_data__(
      subset_array(
        private$.data,
        subset_list,
        drop = FALSE
      )
    )

    # New dimnames attribute of cell dimension needs to be passed to the
    # `.__update_subset__` method in LPJmLMetaData to update the number of
    # cells as well as the (new) firstcell if subsetting by cell
    if (private$.meta$._space_format_ == "cell") {
      cell_dimnames <- self$dimnames()$cell

    } else {
      cell_dimnames <- sort(private$.data) %>%
        format(trim = TRUE, scientific = FALSE, justify = "none")
    }

    # Coordinates tables as lon lat list entries to update meta data
    subset_list <- coords_to_lonlat(subset_list)

    private$.meta$.__update_subset__(
      subset_list,
      cell_dimnames = cell_dimnames
    )

    return(invisible(self))
  }
)

# Assign coordinates (lon, lat) as lon lat entries to update meta data, delete
# coordinate table since its not compatible with update meta data
coords_to_lonlat <- function(subset_list) {

  if (any(c("coords", "coordinates") %in% names(subset_list))) {

  # Get term used for subsetting ("coords" or "coordinates" legit)
  coords <- c("coords", "coordinates")[
    c("coords", "coordinates") %in% names(subset_list)
  ]

  subset_list$lon <- subset_list[[coords]]$lon
  subset_list$lat <- subset_list[[coords]]$lat

  subset_list[[coords]] <- NULL
  }

  subset_list
}

select_space_dims <- function(subset_list) {

  # Get term used for subsetting ("coords" or "coordinates" legit)
  coords <- c("coords", "coordinates")[
    c("coords", "coordinates") %in% names(subset_list)
  ]

  # Get term used for subsetting ("coords" or "coordinates" legit)
  space_dims <- c("cell", "lon", "lat", coords)[
    c("cell", "lon", "lat", coords) %in% names(subset_list)
  ]

  if (length(subset_list[space_dims]) > 0) {
    return(subset_list[space_dims])

  } else {
    return(NULL)
  }
}
