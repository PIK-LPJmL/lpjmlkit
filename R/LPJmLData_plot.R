#' Plot an LPJmLData object
#'
#' Function to plot a time-series or raster map of an [`LPJmLData`]
#' object.
#'
#' @param x [LPJmLData] object
#'
#' @param subset List of array dimension(s) as name/key and
#'   corresponding subset vector as value, e.g. `list(cell = c(27411:27416))`.
#'   More information at [`subset.LPJmLData()`].
#'
#' @param aggregate List of array dimension(s) as name/key and
#'   corresponding aggregation function as value, e.g. `list(band = sum)`.
#'
#' @param raster_extent Optional parameter to crop map display of spatial data.
#'   An \link[raster]{extent} or any object from which an Extent object can be
#'   extracted. Not relevant if `aggregate` includes spatial dimension.
#'
#' @param ... Arguments passed to \link[graphics]{plot} and
#'   \link[raster]{plot}
#'
#' @return No return value; called for side effects.
#'
#'@details
#'Depending on the dimensions of the [LPJmLData] object's internal data array
#' the plot will be a ...
#' * single map plot: more than 8 `"cell"`s or `"lat"` & `"lon"` dimensions
#'   available)
#' * multiple maps plot: length of one time (e.g.`"time"`, `"year"`,
#'   `"month"`) or `"band"` dimension > 1.
#' * time series plot: less than 9 `"cell"`s
#' * lat/lon plot: a subsetted/aggregated `"lat"` or `"lon"` dimension
#'
#' The plot can only handle 2-3 dimensions. Use arguments `subset` and
#' `aggregate` to modify `x$data` to the desired plot type. If more than three
#' dimensions have length > 1,' plot will return an error and suggest to reduce
#' the number of dimensions.
#'
#' *Note that the plot function aims to provide a quick overview of the data
#' rather than create publication-ready graphs.*
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # Plot first 9 years starting from 1901 as a raster plot
#' plot(vegc)
#'
#' # Plot raster with mean over the whole time series
#' plot(vegc,
#'      aggregate = list(time = mean))
#'
#' # Plot only year 2010 as a raster
#' plot(vegc,
#'      subset = list(time = "2010"))
#'
#' # Plot first 10 time steps as global mean time series. Note: Aggregation
#' # across cells is not area-weighted.
#' plot(vegc,
#'      subset = list(time = 1:10),
#'      aggregate = list(cell = mean))
#'
#' # Plot time series for cells with LPJmL index 27410 - 27415 (C indices start
#' #    at 0 in contrast to R indices starting at 1).
#' plot(vegc,
#'      subset = list(cell = 27411:27416))
#'
#' }
#'
#' @md
#' @export plot.LPJmLData
#' @export
plot.LPJmLData <- function(x,
                           subset = NULL,
                           aggregate = NULL,
                           raster_extent = NULL,
                           ...) {
  x$plot(subset,
         aggregate,
         raster_extent,
         ...)
}


# plot method roxygen documentation in LPJmlData.R
LPJmLData$set("private", # nolint start
              ".plot",
              function(subset = NULL,
                       aggregate = NULL,
                       raster_extent = NULL,
                       sub = NULL,
                       ...) {

  time_dims <- strsplit(private$.meta$._time_format_, "_")[[1]]
  space_dims <- strsplit(private$.meta$._space_format_, "_")[[1]]

  # Subset first for better performance
  data_subset <- self$clone(deep = TRUE)
  if (!is.null(subset)) {
    do.call(data_subset$subset, args = subset)
  }

  # Extract ellipsis to later check for passed arguments
  dots <- list(...)

  # Check if available aggregation option is supplied
  if (!all(names(aggregate) %in% c(space_dims,
                                   time_dims,
                                   "band"))) {
    stop(
      "Undefined aggregation dimension ",
      paste0(col_var(names(aggregate)), collapse = ", "),
      " supplied.\nMust be one of ",
      paste0(col_var(space_dims), collapse = ", "),
      ", ",
      paste0(col_var(time_dims), collapse = ", "),
      " or ",
      col_var("band"),
      ".\n"
    )
  }

  # Check for non aggregated space dimension (<= 8 -> time series plots)
  space_len <- names(dim(data_subset)) %>%
    .[!(. %in% names(aggregate))] %>%
    .[. %in% space_dims] %>%
    dim(data_subset)[.]

  descr <- ifelse(is.null(private$.meta$descr),
                  "unknown variable",
                  private$.meta$descr)

  unit <- ifelse(is.null(private$.meta$unit),
                  "-",
                  private$.meta$unit)

  # Plot time series for spatially aggregated data or low cell count
  if (all(space_dims %in% names(aggregate)) ||
      (private$.meta$._space_format_ == "cell" && space_len <= 8) ||
      (private$.meta$._space_format_ == "lon_lat" && any(space_dims %in% names(aggregate)))) { # nolint

    # Add default axis labels to plot
    var_title <- paste0(descr, " [", unit, "]")

    if (!is.null(sub))
      dots$sub <- sub

    if (is.null(dots$ylab)) {
      dots$ylab <- ifelse(is.null(dots$main), "", var_title)
    }

    if (is.null(dots$main)) {
      dots$main <- ifelse(is.null(dots$main), var_title, dots$main)
    }

    # Perform aggregation and plot
    data_only <- aggregate_array(data_subset,
                                 aggregate_list = aggregate,
                                 na.rm = TRUE)

    plot_lines(lpjml_data = data_subset,
               raw_data = data_only,
               aggregate = aggregate,
               dots = dots)
    message(
      col_note(
        "Note: spatial aggregation is not weighted by grid area."
      )
    )

  # Plot map(s) for temporal aggregation or aggregation by band
  } else {

    # Get name of z dimension (layers)
    z_dim <- names(dim(data_subset))[dim(data_subset) >= 1] %>%
      .[!(. %in% names(aggregate))] %>%
      .[!(. %in% space_dims)]

    # If still two z dimensions filter dimensions of length 1
    if (length(z_dim) > 1 && any(dim(data_subset)[z_dim] > 1)) {
      z_dim <- z_dim[z_dim %in% names(dim(data_subset))[dim(data_subset) > 1]]
    } else if (length(z_dim) != 1) {
      z_dim <- "band"
    }

    # If still two z dimensions with length > 1 throw error
    if (length(z_dim) > 1) {
      stop("Too many dimensions. Please reduce via subset or
           aggregate.")
    }

    # Subset first 9 bands or time steps for performance reasons already here
    # (only 9 can be visualized well)
    if (z_dim %in% names(dim(data_subset)) && dim(data_subset)[z_dim] > 9) {
      data_subset$.__set_data__(
        subset_array(
          data_subset$data,
          as.list(stats::setNames(list(seq_len(9)), z_dim)),
          drop = FALSE
        )
      )
    }

    # Aggregate over selected dimensions with provided aggregation function(s)
    data_subset$.__set_data__(
      aggregate_array(data_subset,
                      aggregate_list = aggregate,
                      na.rm = TRUE)
    )

    # Check if a main title has been defined if not create by meta data
    if (is.null(dots$main)) {
      dots$main <- paste0(descr, " [", unit, "]")
    }

    plot_raster(lpjml_data = data_subset,
                raster_extent = raster_extent,
                dots = dots)
  }
}) # nolint end


# Function to create line plots based on lpjml data
#   TODO: requires refactoring # nolint
plot_lines <- function(lpjml_data, # nolint:cyclocomp_linter.
                       raw_data,
                       aggregate,
                       dots) {
  time_dims <- strsplit(lpjml_data$meta$._time_format_, "_")[[1]]
  space_dims <- strsplit(lpjml_data$meta$._space_format_, "_")[[1]]
  dim_names <- names(dim(raw_data))

  if (length(which(dim(raw_data) > 2)) > 2) {
    stop(
      "Too many dimensions for 2D time series plot. Please reduce ",
      paste0(col_var(dim_names), collapse = ", "),
      " to 2.\nMust be at least one temporal dimension (x axis) of ",
      paste0(col_var(time_dims), collapse = ", "),
      ", and could be ",
      col_var("band"),
      " or a temporal dimension, e.g. ",
      col_var("month"),
      " for the y axis",
      ".\n"
    )
  } else if (length(dim(raw_data)) < 2) {
    stop(
      "Only one dimensional data supplied. Data must have two ",
      "dimensions with a minimum of one temporal dimension."
    )
  }

  if (!any(time_dims %in% dim_names) &&
        (lpjml_data$meta$._space_format_ == "cell" ||
           any(space_dims %in% names(aggregate)))) {
    stop(
      "At least one temporal dimension of ",
      paste0(col_var(time_dims), collapse = ", "),
      " has to be provided by the data."
    )
  }

  # Hierarchical ordering of what to display on the x axis
  if (length(time_dims) > 1 && "year" %in% dim_names) {
    x_dim <- "year"
  } else if (length(time_dims) > 1 && "month" %in% dim_names) {
    x_dim <- "month"
  } else if (length(time_dims) > 1 && "month" %in% dim_names) {
    x_dim <- "day"
  } else {
    x_dim <- "time"
  }

  if (lpjml_data$meta$._space_format_ == "lon_lat" &&
        any(dim(raw_data)[space_dims] > 1) &&
        all(dim(raw_data)[time_dims] == 1, na.rm = TRUE)) {
    x_dim <- space_dims[which(dim(raw_data)[space_dims] > 1)]
  }

  # Dimension to be shown in the legend "3rd dimension"
  legend_dim <- dim_names[!dim_names == x_dim]
  if (length(legend_dim) > 1 && any(dim(raw_data)[legend_dim] > 1))
    legend_dim <- legend_dim[legend_dim %in% dim_names[dim(raw_data) > 1]]
  else if (length(legend_dim) > 1 && all(dim(raw_data)[legend_dim]) == 1)
    legend_dim <- ifelse("band" %in% legend_dim, "band", legend_dim[1])


  # Limit plot lines to maximum of 8
  legend_length <- dim(raw_data)[[legend_dim]] %>%
    ifelse(. > 8, 8, .)

  # Subset raw_data that is actually displayed
  raw_data <- subset_array(
    raw_data,
    as.list(stats::setNames(list(seq_len(legend_length)), legend_dim)),
    drop = FALSE
  )

  # Set default axis limits and colors if not provided
  if (is.null(dots$ylim)) {
    dots$ylim <- range(raw_data, na.rm = TRUE, finite = TRUE)
  }

  if (!is.null(dots$col)) {
    cols <- dots$col
    dots <- dots[names(dots) != "col"]
  } else {
    # Use default colors
    cols <- seq_len(legend_length)
  }

  if (is.null(dots$type)) {
    dots$type <- "l"
  }
  if (is.null(dots$xlab)) {
    dots$xlab <- tools::toTitleCase(x_dim)
  }
  # Adjust the axis labels for time and year as x axis
  if (x_dim %in% c("time", "year")) {
    dots$xaxt <- "n"
  }

  # Check if a supported plot type is supplied.
  if (dots$type %in% c("h", "S", "s")) {
    stop(
      "Unsupported plot type ",
      col_var(dots$type),
      " supplied.\nMust be one of ",
      col_var("p"),
      ", ",
      col_var("l"),
      ", ",
      col_var("b"),
      ", ",
      col_var("c"),
      ", ",
      col_var("o"),
      ", or ",
      col_var("n"),
      "."
    )
  }

  withr::with_par(
    new = list(mar = c(12.1, 4.1, 4.1, 4.1), xpd = TRUE),
    code = {
      do.call(
        graphics::plot,
        c(
          x = list(subset_array(
            raw_data,
            as.list(stats::setNames(1, legend_dim))
          )),
          col = cols[1],
          dots
        )
      )

      # Set breaks depending on length of time series
      if (x_dim %in% c("time", "year")) {
        if (lpjml_data$meta$nyear > 100) {
          brks <- 20
        } else if (lpjml_data$meta$nyear > 50) {
          brks <- 10
        } else if (lpjml_data$meta$nyear > 25) {
          brks <- 5
        } else if (lpjml_data$meta$nyear > 10) {
          brks <- 2
        } else {
          brks <- 1
        }

        # Set nstep only if time dimension, leads to month inbetween years
        #   if lpjml_data$meta$nstep > 1
        nstep <- ifelse(x_dim == "time", lpjml_data$meta$nstep, 1)

        # set tickmarks and label for time or year dim
        at_ticks <- seq(1,
                        lpjml_data$meta$nyear * nstep,
                        by = brks * nstep)

        graphics::axis(side = 1,
                       at = at_ticks,
                       labels = dimnames(raw_data)[[x_dim]][at_ticks])
      }

      for (i in cols[-1]) {

        # subset_array for dynamic subsetting of flexible legend_dim
        graphics::lines(subset_array(raw_data,
                                     as.list(stats::setNames(i, legend_dim))),
                        col = cols[i],
                        type = dots$type)
      }

      # Calculate length of longest string in legend to not overlap
      char_len <- dimnames(raw_data)[[legend_dim]] %>%
        .[which.max(nchar(.))]

      # Legend at the bottom left side of the graphic device.
      #   Calculations ensure placement within margins.
      # TODO: replace with withr::with_par for temporarily changing pars # nolint
      graphics::legend(
        x = graphics::par("usr")[1], # nolint:undesirable_function_linter.
        y = graphics::par("usr")[3] - 0.6 * grDevices::dev.size("px")[2] * # nolint:undesirable_function_linter.
          graphics::par("plt")[3] * # nolint:undesirable_function_linter.
          ((graphics::par("usr")[4] - graphics::par("usr")[3]) / # nolint:undesirable_function_linter.
            (
              grDevices::dev.size("px")[2] * (graphics::par("plt")[4] - # nolint:undesirable_function_linter.
                                                graphics::par("plt")[3]) # nolint:undesirable_function_linter.
            )
          ),
        y.intersp = 1.5,
        title = tools::toTitleCase(legend_dim),
        ncol = 2,
        text.width = graphics::strwidth(char_len) * 2,
        col = cols,
        lty = unlist(ifelse(
          dots$type %in% c("l", "b", "c", "o"),
          list(rep(1, legend_length)), list(NULL)
        )),
        pch = unlist(ifelse(
          dots$type %in% c("p", "b", "o"), list(rep(1, legend_length)), list(NULL)
        )),
        legend = dimnames(raw_data)[[legend_dim]][cols],
        bty = "n"
      )
    }
  )
  # Create grid, special case for time, year because of specified x axis
  if (x_dim %in% c("time", "year")) {
    graphics::abline(v = at_ticks,
                     col = "lightgray",
                     lty = "dotted",
                     lwd = graphics::par("lwd")) # nolint:undesirable_function_linter.
    graphics::grid(nx = NA, ny = NULL)
  } else {
    graphics::grid()
  }
}


# Function to create raster plots based on lpjml data
plot_raster <- function(lpjml_data,
                        raster_extent,
                        dots) {
  # Create raster and crop if extent is provided. Otherwise, automatic
  #   cropping via as_raster functionality is used.
  data_ras <- lpjml_data %>%
    as_raster() %>%
    # Crop and extend to crop data where raster_extent is not covered and extend
    # again to full raster_extent
    `if`(
      !is.null(raster_extent),
      raster::crop(raster::extend(., y = raster_extent), y = raster_extent),
      .
    )

  # If layer name equals variable name avoid double information
  if (!all(lpjml_data$meta$variable == names(data_ras))) {
    dots$main <- paste0(dots$main, ": ", gsub("X", "", names(data_ras)))
  }

  if (is.null(dots$zlim)) {
    zlim <- NULL
  } else {
    zlim <- dots$zlim
    dots$zlim <- NULL
  }

  # Set color scale of raster plot
  if (is.null(dots$col)) {
    map_col <- rev(grDevices::terrain.colors(255))

    # Adjust color scale and zlim for plots with positive and negative values.
    if (length(intersect(
      which(raster::minValue(data_ras) < -1e-7),
      which(raster::maxValue(data_ras) > 1e-7)
    ))) {
      # Create color mapping for 255 colors for every layer
      map_col <- replicate(
        n = raster::nlayers(data_ras),
        expr = rev(grDevices::terrain.colors(255)), simplify = FALSE
      )

      # Set zlim for negative and positive values
      zlim <- mapply( # nolint:undesirable_function_linter.
        FUN = c, raster::minValue(data_ras),
        raster::maxValue(data_ras), SIMPLIFY = FALSE
      )

      # Get indices of layers with negative and positive values
      neg_and_pos <- intersect(
        which(raster::minValue(data_ras) < 0),
        which(raster::maxValue(data_ras) > 0)
      )

      # Use specific colour ramp to layers with negative and positive values
      map_col[neg_and_pos] <-
        lapply(zlim[neg_and_pos], FUN = create_color_scale) # nolint:undesirable_function_linter
    }
  } else {
    map_col <- dots$col
    dots$col <- NULL
  }

  # Set plot arrangement for multiple raster layers
  nr_nc <- switch(
    as.character(raster::nlayers(data_ras)), "1" = c(1, 1),
    "2" = c(1, 2),
    "3" = c(2, 2),
    "4" = c(2, 2),
    "5" = c(3, 2),
    "6" = c(3, 2),
    c(3, 3)
  )

  # Plot raster for multiple or single layer(s)
  if (is.list(zlim) || is.list(map_col)) {
    dots <- lapply(
      seq_len(length(zlim)),
      function(i, x) lapply(x, "[", i),
      x = dots
    )
    withr::with_par(new = list(mfrow = nr_nc),
      code = invisible(
        mapply(FUN = call_raster_plot, # nolint:undesirable_function_linter.
          x = lapply(
            X = seq_len(min(raster::nlayers(data_ras), 9)),
            FUN = function(x) {
              return(raster::subset(data_ras, x))
            }
          ),
          col = map_col, zlim = zlim, dots = dots
        )
      )
    )
  } else {
    dots$nc <- nr_nc[2]
    dots$nr <- nr_nc[1]
    call_raster_plot(x = data_ras, col = map_col, zlim = zlim, dots = dots)
  }
}


# Create color scale for raster plots
create_color_scale <- function(zlim,
                               breaks = 255) {
  breaks <- breaks - 1
  # Calculate difference of range for zlim
  min_max <- sum(abs(zlim))

  # Generate colors for negative (neg) and positive (pos) values
  neg <- grDevices::colorRampPalette(rev(c(
    "#f7fbff", "#deebf7",
    "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5",
    "#08519c", "#08306b"
  )))(round(breaks * abs(zlim[1]) / min_max))
  pos <- grDevices::colorRampPalette(c(
    "#fff5f0", "#fee0d2",
    "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d",
    "#a50f15", "#67000d"
  ))(round(breaks * zlim[2] / min_max))

  # Colour bar with negative value colors, 0 = white, and positive value colors
  c(neg, "white", pos)
}


# Wrapper for raster::plot to add country map overlay
call_raster_plot <- function(x, col, zlim, dots) {
  # do.call to use dots list as ellipsis, addfun is a workaround to use
  # country map overlay for every plot
  do.call(
    raster::plot,
    c(
      list(
        x = x,
        addfun = function() maps::map(add = TRUE, lwd = 0.3),
        col = col, zlim = zlim
      ),
      dots
    )
  )
}
