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
    # Subset months if defined
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

  if (nstep == 365) {
    # Daily data: YYYY-MM-DD
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

    time_dimnames <- paste(
      rep(years, each = length(dd)),
      rep(d_mmdd, times = length(years)),
      sep = "-"
    )
  } else if (nstep == 12) {
    # Monthly data: YYYY-MM-LastDayOfMonth
    m_mmdd <- paste(mm, ndays_in_month, sep = "-")

    time_dimnames  <- paste(
      rep(years, each = length(ndays_in_month)),
      rep(m_mmdd, times = length(years)),
      sep = "-"
    )
  } else if (nstep == 1) {
    # Annual data: YYYY-12-31
    time_dimnames <- paste(years, 12, 31, sep = "-")
  } else {
    # Currently no support for other (special) nstep cases
    stop("Invalid nstep: ", nstep, "\nnstep has to be 1, 12 or 365")
  }

  return(time_dimnames)
}

#' Split date strings into years, months and days
#'
#' Splits one or several date strings into a list of unique days, months and
#' years.
#'
#' @param time_names Character vector with one or several date strings in the
#'   form YYYY-MM-DD.
#'
#' @return List containing unique `year`, `month` and `day` values included in
#'   `time_names`.
#'
#' @examples
#' \dontrun{
#'  time_names <- split_time_names(c("2024-11-25", "2024-11-26"))
#'  time_names
#'  # $year
#'  # [1] "2024"
#'  # $month
#'  # [1] "11"
#'  # $day
#'  # [1] "25" "26"
#' }
#'
#' @md
#' @export
split_time_names <- function(time_names) {

  # Split time string "year-month-day" into year, month, day integer vector
  time_split <- regmatches(
    time_names,
    regexec("([-]?[[:digit:]]+)-([[:digit:]]+)-([[:digit:]]+)", time_names)
  ) %>% lapply(function(x) as.character(as.integer(x[-1])))

  # Create corresponding dimnames for disaggregated array by unique entry
  matrix(unlist(time_split),
         nrow = length(time_split),
         byrow = TRUE,
         dimnames = list(seq_along(time_split),
                         c("year", "month", "day"))) %>%
    apply(2, unique) %>%
    as.list() %>%
    return()
}
