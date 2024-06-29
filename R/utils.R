#' Reformat date to SDTM conventions
#'
#' @param x The date in POSIX format
#' @return Date-time formatted as xxxx-xx-xxTxx:xx:xx
#' @keywords internal
reformat_date <- function(x) {
  return(format(x, "%Y-%m-%dT%H:%M"))
}

#' Convert all DTC fields into ISO 8601 string format
#'
#' Change all columns in the input data frame that end with 'DTC' from POSIct to
#' character using the ISO 8601 format.
#' @param obj A data frame.
#' @return A data frame.
#' @seealso [lubrify_dates()]
#' @keywords internal
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}

rich_sampling_scheme <- tibble::tribble(
  ~time,  ~PCTPT,
  0,      "PREDOSE",
  0.5,    "HOUR 0.5",
  1,      "HOUR 1",
  1.5,    "HOUR 1.5",
  2,      "HOUR 2",
  3,      "HOUR 3",
  4,      "HOUR 4",
  6,      "HOUR 6",
  8,      "HOUR 8",
  10,     "HOUR 10",
  12,     "HOUR 12",
  24,     "HOUR 24",
  48,     "HOUR 48",
  72,     "HOUR 72",
  96,     "HOUR 96",
  144,    "HOUR 144",
  168,    "HOUR 168"
)
