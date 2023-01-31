# Create a vector of dates to be used as e.g. names of a time dimension. Dates
# are written with as.characters, not as.Date, in order to avoid issues with
# leap years. By default, for annual/monthly outputs, the function returns the
# last day of the year/month, as these are the days when LPJmL writes
# annual/montly data outputs.
create_time_names <- function(
  nstep = 365,
  years = 2000,
  months = NULL,
  days = NULL
) {

  # Number of days per month.
  ndays_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) %>%
    # Subset months if defined.
    { # nolint
      if (is.null(months)) . else .[months]
    }

  # Days and months in two-digits format (e.g. "01").
  dd <- unlist(lapply(ndays_in_month, FUN = seq_len)) %>%
    # Subset days if defined.
    { # nolint
      if (!is.null(days)) .[which(. %in% days)] else .
    } %>%
    sprintf("%02d", .)

  mm <- {if (is.null(months)) seq_len(12) else months} %>% #nolint
    sprintf("%02d", .)

  # Daily data: YYYY-MM-DD
  if (nstep == 365) {
    d_mmdd <- paste(
      rep(x = mm, times = {
        # Cases of months or days being subsetted or not.
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

  # Monthly data: YYYY-MM-LastDayOfMonth
  } else if (nstep == 12) {
    m_mmdd <- paste(mm, ndays_in_month, sep = "-")

    time_dimnames  <- paste(
      rep(years, each = length(ndays_in_month)),
      rep(m_mmdd, times = length(years)),
      sep = "-"
    )

  # Annual data: YYYY-12-31
  } else if (nstep == 1) {
    time_dimnames <- paste(years, 12, 31, sep = "-")

  # Currently no support for other (special) nstep cases.
  } else {
    stop(paste0("Invalid nstep: ", nstep, "\nnstep has to be 1, 12 or 365"))
  }

  return(time_dimnames)
}

split_time_names <- function(time_names) {

  # Split time string "year-month-day" into year, month, day int vector.
  time_split <- strsplit(time_names, "-") %>%
    lapply(function(x) as.character(as.integer(x)))

  # Create corresponding dimnames for disaggregated array by unique entry.
  matrix(unlist(time_split),
         nrow = length(time_split),
         byrow = TRUE,
         dimnames = list(seq_along(time_split),
                         c("year", "month", "day"))) %>%
    apply(2, unique) %>%
    as.list() %>%
  return()
}
