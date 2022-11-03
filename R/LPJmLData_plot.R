#' Plot LPJmLData class
#'
#' Function to plot a time-series or raster map of a LPJmLData
#' object. This function is only supposed to give an overview of
#' the data and not to create publication ready graphics.
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27415)`, more information at
#' \link[lpjmlKit]{subset}.
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#' @param raster_extent if `aggregate` does not include space dim
#' [extent object](\link[raster]{extent}), or any object from
#' which an Extent object can be extracted
#' @param ... arguments forwarded to graphics::plot() and
#' raster::plot()
#'
#' @return NULL
#' @export
#'
#' @examples
plot <- function(x, ...) {
  x$plot(...)
}

LPJmLData$set("public", "plot",
              # TODO: INSERT ROXYGEN DOC
              function(subset = NULL,
                       aggregate = NULL,
                       raster_extent = NULL,
                       ...) {
  time_dims <- strsplit(self$meta$time_format, "_")[[1]]
  space_dims <- strsplit(self$meta$space_format, "_")[[1]]
  # do subsetting first for better performance
  data_subset <- self$clone()
  if (!is.null(subset)) {
    do.call(data_subset$subset, args = subset)
  }
  # extract ellipsis to later check for passed arguments
  dots <- list(...)

  # check if available aggregation option is supplied
  if (!all(names(aggregate) %in% c(space_dims,
                                   time_dims,
                                   "band"))) {
    stop(
      paste0(
        "\u001b[0m",
        "Undefined aggregation dimension ",
        "\u001b[34m",
        paste0(names(aggregate), collapse = ", "),
        "\u001b[0m",
        " supplied.\nMust be one of ",
        "\u001b[34m",
        paste0(space_dims, collapse = ", "),
        "\u001b[0m",
        ", ",
        "\u001b[34m",
        paste0(time_dims, collapse = ", "),
        "\u001b[0m",
        " or ",
        "\u001b[34m",
        "band",
        "\u001b[0m",
        ".\n"
      )
    )
  }

  # check for non aggregated space dimension <= 8 -> time series plots
  space_len <- names(dim(data_subset)) %>%
    .[!(. %in% names(aggregate))] %>%
    .[. %in% space_dims] %>%
    dim(data_subset)[.]

  descr <- ifelse(is.null(self$meta$descr),
                  "unknown variable",
                  self$meta$descr)
  unit <- ifelse(is.null(self$meta$unit),
                  "-",
                  self$meta$unit)
  # for spatial aggregation plot time series
  if (all(space_dims %in% names(aggregate)) ||
      (self$meta$space_format == "cell" && space_len <= 8) ||
      (self$meta$space_format == "lon_lat" && any(space_dims %in% names(aggregate)))) { # nolint
    #add default axes labels to plot.
    var_title <- paste0(descr, " [", unit, "]")
    if (is.null(dots$ylab)) {
      dots$ylab <- ifelse(is.null(dots$main), "", var_title)
    }
    if (is.null(dots$main)) {
      dots$main <- ifelse(is.null(dots$main), var_title, dots$main)
    }
    # perform aggregation and plot
    data_only <- aggregate_array(data_subset,
                                 aggregate_list = aggregate,
                                 na.rm = TRUE)
    data_subset$._plot_by_band(data = data_only, dots = dots)
    message(
      paste0(
        "\u001b[33;3m",
        "Note: spatial aggregation is not weighted by grid area.",
        "\u001b[0m"
      )
    )
  # for temporal or aggregation by band plot a maps
  } else {
    # get name of z dimension (layers)
    z_dim <- names(dim(data_subset))[dim(data_subset) >= 1] %>%
      .[!(. %in% names(aggregate))] %>%
      .[!(. %in% space_dims)]

    # if still two z dimensions filter dimension of length 1
    if (length(z_dim) > 1) {
      z_dim <- z_dim[z_dim %in% names(dim(data_subset))[dim(data_subset) > 1]]
    } else if (length(z_dim) == 0) {
      z_dim <- "band"
    }

    # if two two z dimensions with length > 1 throw error
    if (length(z_dim) > 1) {
      stop("Too many dimensions. Please reduce via subset or
           aggregate.")
    }
    # subset first 9 (later) raster layers (only 9 can be visualized well)
    #   for performance reasons already here
    if (dim(data_subset)[z_dim] > 9) {
      data_subset$data <- subset_array(data_subset$data,
                                       as.list(setNames(list(seq_len(9)), z_dim)), # nolint
                                       force_idx = TRUE)
    }

    # create plot title if not provided
    if (is.null(dots$main)) {
      var_title <- paste0(descr, " [", unit, "]")
    } else {
      var_title <- dots$main
      dots$main <- NULL
    }

    # aggregate over selected dimensions and corresponding fun of aggregate
    data_subset$data <- aggregate_array(data_subset,
                                        aggregate_list = aggregate,
                                        na.rm = TRUE)

    # create raster and crop if extent ist provided, otherwise automatic
    #   cropping via as_raster functionality is used
    data_ras <- data_subset %>%
      as_raster() %>%
      `if`(!is.null(raster_extent), raster::crop(., y = raster_extent), .)
    # do.call to use dots list as ellipsis, addfun is a workaround to use
    #   country map overlay for every plot
    do.call(raster::plot,
            c(list(x = data_ras,
                   main = paste0(var_title, ": ", gsub("X", "", names(data_ras))), # nolint
                   addfun = function() maps::map(add = TRUE, lwd = 0.3)),
              dots))
  }
})


# helper function to draw time series plots
LPJmLData$set("public",
              "._plot_by_band",
              function(data, dots) {
  is_sequential <- function(x) all(diff(as.integer(x)) == 1)
  time_dims <- strsplit(self$meta$time_format, "_")[[1]]
  space_dims <- strsplit(self$meta$space_format, "_")[[1]]
  dim_names <- names(dim(data))

  if (length(which(dim(data) > 2)) > 2) {
    stop(
      paste0(
        "\u001b[0m",
        "Too many dimensions for 2D time series plot. Please reduce ",
        "\u001b[34m",
        paste0(dim_names, collapse = ", "),
        "\u001b[0m",
        " to 2.\nMust be at least one temporal dimension (x axis) of ",
        "\u001b[34m",
        paste0(time_dims, collapse = ", "),
        "\u001b[0m",
        ", and could be ",
        "\u001b[34m",
        "band",
        "\u001b[0m ",
        "or a temporal dimension, e.g. \u001b[34mmonth\u001b[0m for the y axis",
        ".\n"
      )
    )
  } else if (length(dim(data)) < 2) {
    stop(paste0("Only one dimensional data supplied. Data must have two ",
                "dimensions with a minimum of one temporal dimension."))
  }

  if (!any(time_dims %in% dim_names)) {
    stop(paste0("At least one temporal dimension of ",
                "\u001b[34m",
                 paste0(time_dims, collapse = ", "),
                 "\u001b[0m",
                 " has to be provided by the data."))
  }

  # hierarchical ordering of what to display on the x axis
  if (length(time_dims) > 1 && "year" %in% dim_names) {
    x_dim <- "year"
  } else if (length(time_dims) > 1 && "month" %in% dim_names) {
    x_dim <- "month"
  } else if (length(time_dims) > 1 && "month" %in% dim_names) {
    x_dim <- "day"
  } else {
    x_dim <- "time"
  }

  if (self$meta$space_format == "lon_lat" &&
      any(dim(data)[space_dims] > 1) &&
      all(dim(data)[time_dims] == 1, na.rm = TRUE)) {
    x_dim <- space_dims[which(dim(data)[space_dims] > 1)]
  }

  # dimension to be shown in the legend "3rd dimension"
  legend_dim <- dim_names[!dim_names == x_dim]
  if (length(legend_dim) > 1)
    legend_dim <- legend_dim[legend_dim %in% dim_names[dim(data) > 1]]

  # limit plot lines to maximum of 8
  legend_length <- dim(data)[[legend_dim]] %>%
    ifelse(. > 8, 8, .)

  # subset data that is actually displayed
  data <- subset_array(
    data,
    as.list(setNames(list(seq_len(legend_length)), legend_dim)),
    force_idx = TRUE,
    drop = FALSE
  )

  # set default Axis lims and colors if not provided
  if (is.null(dots$ylim)) {
    dots$ylim <- range(data, na.rm = TRUE, finite = TRUE)
  }
  # if (is.null(dots$xlim)) {
  #   if (x_dim != "time") {
  #     xlabels <- as.numeric(dimnames(data)[[x_dim]])
  #     if (x_dim != "year") {
  #       dots$xlim <- range(xlabels)
  #     } else {
  #       dots$xlim <- range(xlabels - (xlabels[1] + 1))
  #     }
  #   }
  # }
  if (!is.null(dots$col)) {
    col <- dots$col
    dots <- dots[names(dots) != "col"]
  }
  if (is.null(dots$type)) {
    dots$type <- "l"
  }
  if (is.null(dots$xlab)) {
    dots$xlab <- tools::toTitleCase(x_dim)
  }
  # for time and year as x axis adjust the axis labels
  if (x_dim %in% c("time", "year")) {
    dots$xaxt <- "n"
  }

  # check if a supported plot type is supplied.
  if (dots$type %in% c("h", "S", "s")) {
    stop(cat(
      paste0(
        "\u001b[0m",
        "Unsupported plot type ",
        "\u001b[34m",
        dots$type,
        "\u001b[0m",
        " supplied.\nMust be one of ",
        "\u001b[34m",

        "p",
        "\u001b[0m",
        ", ",
        "\u001b[34m",
        "l",
        "\u001b[34m",
        "b",
        "\u001b[0m",
        ", ",
        "\u001b[34m",
        "c",
        "\u001b[0m",
        ", ",
        "\u001b[34m",
        "o",
        "\u001b[0m",
        ", ",
        "\u001b[0m",
        " or ",
        "\u001b[34m",
        "n",
        "\u001b[0m",
        ".\n"
      )
    ))
  }

  # use default colours
  cols <- seq_len(legend_length)
  opar <- graphics::par(mar = c(12.1, 4.1, 4.1, 4.1), xpd = TRUE)

  # do.call for use of ellipsis via dots list
  #   subset_array for dynamic subsetting of flexible legend_dim
  #   force_idx to use index instead of numeric year or lat, lon for subsetting
  do.call(graphics::plot,
          c(x = list(subset_array(data,
                                  as.list(setNames(1, legend_dim)),
                                  force_idx = TRUE)),
            col = cols[1],
            dots))

  # depending on length of time series set breaks accordingly
  if (x_dim %in% c("time", "year")) {
    if (self$meta$nyear > 100) {
      brks <- 20
    } else if (self$meta$nyear > 50) {
      brks <- 10
    } else if (self$meta$nyear > 25) {
      brks <- 5
    } else if (self$meta$nyear > 10) {
      brks <- 2
    } else {
      brks <- 1
    }

    # set nstep only if time dimension, leads to month inbetween years
    #   if self$meta$nstep > 1
    nstep <- ifelse(x_dim == "time", self$meta$nstep, 1)

    # set tickmarks and label for time or year dim
    at_ticks <- seq(1,
                    self$meta$nyear * nstep,
                    by = brks * nstep)
    graphics::axis(side = 1,
                   at = at_ticks,
                   labels = dimnames(data)[[x_dim]][at_ticks])
  }

  for (i in cols[-1]) {
  # subset_array for dynamic subsetting of flexible legend_dim
  #   force_idx to use index instead of numeric year or lat, lon for subsetting
    graphics::lines(subset_array(data,
                                 as.list(setNames(i, legend_dim)),
                                 force_idx = TRUE),
                    col = cols[i],
                    type = dots$type)
  }
  # calculate length of longest string in legend to not overlap
  char_len <- dimnames(data)[[legend_dim]] %>%
    .[which.max(nchar(.))]

  # legend at the bottom left side of the graphic device.
  #   calculations ensure placement within margins.
  legend(
    x = graphics::par("usr")[1],
    y = graphics::par("usr")[3] - 0.6 * grDevices::dev.size("px")[2] *
      graphics::par("plt")[3] *
      ((graphics::par("usr")[4] - graphics::par("usr")[3]) /
         (
           grDevices::dev.size("px")[2] * (graphics::par("plt")[4] -
                                             graphics::par("plt")[3])
         )
      ),
    y.intersp = 1.5,
    title = tools::toTitleCase(legend_dim),
    ncol = 2,
    text.width = strwidth(char_len) * 2,
    col = cols,
    lty = unlist(ifelse(
      dots$type %in% c("l", "b", "c", "o"),
      list(rep(1, legend_length)), list(NULL)
    )),
    pch = unlist(ifelse(
      dots$type %in% c("p", "b", "o"), list(rep(1, legend_length)), list(NULL)
    )),
    legend = dimnames(data)[[legend_dim]][cols],
    bty = "n"
  )
  graphics::par(opar) %>% invisible()
  # create grid, special case for time, year because of specified x axis
  if (x_dim %in% c("time", "year")) {
    graphics::abline(v = at_ticks,
                     col = "lightgray",
                     lty = "dotted",
                     lwd = par("lwd"))
    graphics::grid(nx = NA, ny = NULL)
  } else {
    graphics::grid()
  }
})