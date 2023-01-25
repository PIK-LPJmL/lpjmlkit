#' Plot a LPJmLData object
#'
#' Function to plot a time-series or raster map of a [`LPJmLData`]
#' object. *The intent is to provide a quick overview of the data,
#' not to create publication-ready graphs.*
#'
#' @param x [LPJmLData] object
#'
#' @param subset list of array dimension(s) as name/key and
#' corresponding subset vector as value, e.g.
#' `list(cell = c(27411:27416)`, more information at
#' [`subset.LPJmLData`].
#'
#' @param aggregate list of array dimension(s) as name/key and
#' corresponding aggregation function as value, e.g.
#' `list(band = sum)`.
#'
#' @param raster_extent if `aggregate` does not include space dim
#' \link[raster]{extent}, or any object from
#' which an Extent object can be extracted.
#'
#' @param ... arguments forwarded to \link[graphics]{plot} and
#' \link[raster]{plot}
#'
#' @examples
#' \dontrun{
#'
#' vegc <- read_io(filename = "./vegc.bin.json")
#'
#' # plots first 9 years starting from 1901 as a raster plot
#' plot(vegc)
#'
#' # plots raster with mean over whole time series
#' plot(vegc,
#'      aggregate = list(time=mean))
#'
#' # plots only year 2010 as raster
#' plot(vegc,
#'      subset = list(time = "2010"))
#'
#' # plots only year 2010 as raster
#' plot(vegc,
#'      subset = list(time = "2010")
#'
#' # plots first 10 time steps as global mean time series
#' plot(vegc,
#'      subset = list(time = 1:10),
#'      aggregate = list(cell = mean))
#'
#' # plots time series for cells with LPJmL index 27410 - 27414 (C indices start
#' #    at 0 in contrast to R indices - at 1)
#' plot(vegc,
#'      subset = list(cell = 27411:27416))
#'
#' }
#'
#' @md
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
LPJmLData$set("private",
              ".plot",
              function(subset = NULL,
                       aggregate = NULL,
                       raster_extent = NULL,
                       ...) {

  time_dims <- strsplit(private$.meta$._time_format_, "_")[[1]]
  space_dims <- strsplit(private$.meta$._space_format_, "_")[[1]]

  # do subsetting first for better performance
  data_subset <- self$clone(deep = TRUE)
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

  descr <- ifelse(is.null(private$.meta$descr),
                  "unknown variable",
                  private$.meta$descr)

  unit <- ifelse(is.null(private$.meta$unit),
                  "-",
                  private$.meta$unit)

  # for spatial aggregation plot time series
  if (all(space_dims %in% names(aggregate)) ||
      (private$.meta$._space_format_ == "cell" && space_len <= 8) ||
      (private$.meta$._space_format_ == "lon_lat" && any(space_dims %in% names(aggregate)))) { # nolint

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

    plot_by_band(lpjml_data = data_subset,
                 raw_data = data_only,
                 aggregate = aggregate,
                 dots = dots)
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

    # if still two z dimensions filter dimensions of length 1
    if (length(z_dim) > 1 && any(dim(data_subset)[z_dim] > 1)) {
      z_dim <- z_dim[z_dim %in% names(dim(data_subset))[dim(data_subset) > 1]]
    } else if (length(z_dim) != 1) {
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
      data_subset$.__set_data__(
        subset_array(data_subset$data,
                     as.list(stats::setNames(list(seq_len(9)), z_dim)))
      )
    }

    # create plot title if not provided
    if (is.null(dots$main)) {
      var_title <- paste0(descr, " [", unit, "]")
    } else {
      var_title <- dots$main
      dots$main <- NULL
    }

    # aggregate over selected dimensions and corresponding fun of aggregate
    data_subset$.__set_data__(
      aggregate_array(data_subset,
                      aggregate_list = aggregate,
                      na.rm = TRUE)
    )

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
#   TODO: requires refactoring # nolint
plot_by_band <- function(lpjml_data, # nolint:cyclocomp_linter.
                         raw_data,
                         aggregate,
                         dots) {
  time_dims <- strsplit(lpjml_data$meta$._time_format_, "_")[[1]]
  space_dims <- strsplit(lpjml_data$meta$._space_format_, "_")[[1]]
  dim_names <- names(dim(raw_data))

  if (length(which(dim(raw_data) > 2)) > 2) {
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
  } else if (length(dim(raw_data)) < 2) {
    stop(paste0("Only one dimensional data supplied. Data must have two ",
                "dimensions with a minimum of one temporal dimension."))
  }

  if (!any(time_dims %in% dim_names) &&
      (lpjml_data$meta$._space_format_ == "cell" ||
       any(space_dims %in% names(aggregate)))) {
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

  if (lpjml_data$meta$._space_format_ == "lon_lat" &&
      any(dim(raw_data)[space_dims] > 1) &&
      all(dim(raw_data)[time_dims] == 1, na.rm = TRUE)) {
    x_dim <- space_dims[which(dim(raw_data)[space_dims] > 1)]
  }

  # dimension to be shown in the legend "3rd dimension"
  legend_dim <- dim_names[!dim_names == x_dim]
  if (length(legend_dim) > 1 && any(dim(raw_data)[legend_dim]) > 1)
    legend_dim <- legend_dim[legend_dim %in% dim_names[dim(raw_data) > 1]]
  else if (length(legend_dim) > 1 && all(dim(raw_data)[legend_dim]) == 1)
    legend_dim <- ifelse("band" %in% legend_dim, "band", legend_dim[1])


  # limit plot lines to maximum of 8
  legend_length <- dim(raw_data)[[legend_dim]] %>%
    ifelse(. > 8, 8, .)

  # subset raw_data that is actually displayed
  raw_data <- subset_array(
    raw_data,
    as.list(stats::setNames(list(seq_len(legend_length)), legend_dim)),
    drop = FALSE
  )

  # set default axis lims and colors if not provided
  if (is.null(dots$ylim)) {
    dots$ylim <- range(raw_data, na.rm = TRUE, finite = TRUE)
  }

  if (!is.null(dots$col)) {
    cols <- dots$col
    dots <- dots[names(dots) != "col"]
  } else {
    # use default colours
    cols <- seq_len(legend_length)
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

  opar <- graphics::par(mar = c(12.1, 4.1, 4.1, 4.1), xpd = TRUE) # nolint:undesirable_function_linter.

  # do.call for use of ellipsis via dots list
  #   subset_array for dynamic subsetting of flexible legend_dim
  do.call(graphics::plot,
          c(x = list(subset_array(raw_data,
                                  as.list(stats::setNames(1, legend_dim)))),
            col = cols[1],
            dots))

  # depending on length of time series set breaks accordingly
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

    # set nstep only if time dimension, leads to month inbetween years
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

  # calculate length of longest string in legend to not overlap
  char_len <- dimnames(raw_data)[[legend_dim]] %>%
    .[which.max(nchar(.))]

  # legend at the bottom left side of the graphic device.
  #   calculations ensure placement within margins.
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
  graphics::par(opar) %>% invisible() # nolint:undesirable_function_linter.

  # create grid, special case for time, year because of specified x axis
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
