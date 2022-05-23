#' @title Create a character vector of dates.
#'
#' @description Create a vector of dates to be used as
#' e.g. names of a time dimension. Dates are written as.characters, not as.Date,
#' in order to avoid issues with leap years. By default, for annual/monthly outputs,
#' it returns the last day of the year/month, as these are the days when LPJmL
#' writes annual/montly data outputs.
#' @return A character vector of dates in the format "YYYY-MM-DD".
#' @param nstep An integer value defining the time step of the output file.
#' Valid values are 1 (yearly), 12 (monthly), 365 (daily).
#' @param years An integer vector of (sequential or non-sequential) years.
#' @examples
#' @details
#' @seealso strptime, as.Date, format
#' @export

create_time_names <- function(
  nstep = 365,
  years = 2000,
  months = NULL,
  days = NULL
) {

  # Number of days per month
  ndays_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) %>%
    # subset months if defined
    {
      if (is.null(months)) . else .[months]
    }

  # Days and months in two-digits format (e.g. "01")
  dd <- unlist(lapply(ndays_in_month, FUN = seq_len)) %>%
    # subset days if defined
    {
      if (!is.null(days)) .[which(. %in% days)] else .
    } %>%
    sprintf("%02d", .)
  mm <- {if (is.null(months)) seq_len(12) else months} %>% #nolint
    sprintf("%02d", .)

  # daily data: YYYY-MM-DD
  if (nstep == 365) {
    d_mmdd <- paste(
      rep(x = mm, times = {
        # cases of months or days being subsetted or not
        if (is.null(days)) { #nolint
          ndays_in_month
        } else if (!is.null(days) && is.null(months)) {
          rep(length(days), 12)
        } else {
          rep(length(days), length(months))
        }
      }
      ), dd, sep = "-"
    )
    time_dimnames <- paste(rep(years, each = length(dd)),
                        rep(d_mmdd, times = length(years)), sep = "-")

  # monthly data: YYYY-MM-LastDayOfMonth
  } else if (nstep == 12) {
    m_mmdd <- paste(mm, ndays_in_month, sep = "-")
    time_dimnames  <- paste(
      rep(years, each = length(ndays_in_month)),
      rep(m_mmdd, times = length(years)),
      sep = "-"
    )

  # yearly data: YYYY-12-31
  } else if (nstep == 1) {
    time_dimnames <- paste(years, 12, 31, sep = "-")

  # currently no support for other (special) nstep cases
  } else {
    stop(paste0("Invalid nstep: ", nstep, "\nnstep has to be 1, 12 or 365"))
  }
  return(time_dimnames)
}

split_time_names <- function(time_names) {
  # split time string "year-month-day" into year, month, day int vector
  time_split <- strsplit(time_names, "-") %>%
    lapply(as.integer)

  # create corresponding dimnames for disaggregated array by unique entry
  matrix(unlist(time_split),
         nrow = length(time_split),
         byrow = TRUE,
         dimnames = list(seq_along(time_split),
                         c("year", "month", "day"))) %>%
    apply(2, unique) %>%
    as.list() %>%
  return()
}



# ------------------------------------ #
# Other time-related functions

# Convert day-of-the-year (DOY) to date "YYYY-MM-DD"
doy_to_date <- function(doy  = NULL,
                        year = NULL
) {
    if (length(doy) != length(year)) stop("doy and year have different length")

    date <- strptime(paste(year, doy), format = "%Y %j")
    return(date)
}

# Convert date "YYYY-MM-DD" to day-of-the-year (DOY)
date_to_doy <- function(date = "2010-01-29"
) {
    as.integer(format(as.Date(date), "%j"))
}

# Create sequence of dates and return as character
seq_dates <- function(start_date = "1980-01-01",
                      end_date   = "1980-12-31",
                      step       = "day") {
    as.character(
        seq.Date(from = as.Date(start_date),
        to   = as.Date(end_date),
        by   = step)
    )
}
