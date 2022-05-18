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
  years = 2000
) {

  # Number of days per month
  ndays_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Days and months in two-digits format (e.g. "01")
  dd <- sprintf("%02d", unlist(lapply(ndays_in_month, FUN = seq_len)))
  mm <- sprintf("%02d", seq_len(12))

  # daily data: YYYY-MM-DD
  d_mmdd     <- paste(rep(mm, times = ndays_in_month), dd, sep = "-")
  d_yyyymmdd <- paste(rep(years, each = 365),
                      rep(d_mmdd, times = length(years)), sep = "-")
  # monthly data: YYYY-MM-LastDayOfMonth
  m_mmdd     <- paste(mm, ndays_in_month, sep = "-")
  m_yyyymmdd  <- paste(rep(years, each = 12),
                      rep(m_mmdd, times = length(years)), sep = "-")
  # yearly data: YYYY-12-31
  y_yyyymmdd <- paste(years, 12, 31, sep = "-")

  # Select time vector according to nstep
  time_dimnames <- switch(
    as.character(nstep),
    "365" = d_yyyymmdd,
    "12"  = m_yyyymmdd,
    "1"   = y_yyyymmdd,
    stop(paste0("Invalid nstep: ", nstep, "\nnstep has to be 1, 12 or 365"))
  )

  return(time_dimnames)
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