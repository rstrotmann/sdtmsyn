#' Recode SEX field in a data frame
#'
#' This function recodes the SEX field in a data frame. All numerical values are
#' kept while "M" is recoded to 0 and "F" to 1. If your downstream analysis
#' requires different coding, please manually re-code.
#' @param obj The data.frame containing a SEX field
#' @return The output data frame
#' @import dplyr
#' @keywords internal
recode_sex <- function(obj) {
  obj %>%
    mutate(SEX = as.numeric(
      case_match(as.character(.data$SEX),
                 "M" ~ 0, "F" ~ 1, "1" ~ 1, "0" ~ 0,
                 # "男" ~ 0, "女" ~ 1,
                 "\u7537" ~ 0, "\u5973" ~ 1,
                 .default = NA)
    ))
}

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
  ~time,  ~PCTPT,     ~window,
  0,      "PREDOSE",  1,
  0.5,    "HOUR 0.5", 5/60,
  1,      "HOUR 1",   5/60,
  1.5,    "HOUR 1.5", 10/60,
  2,      "HOUR 2",   10/60,
  3,      "HOUR 3",   15/60,
  4,      "HOUR 4",   15/60,
  6,      "HOUR 6",   30/60,
  8,      "HOUR 8",   30/60,
  10,     "HOUR 10",  1,
  12,     "HOUR 12",  1,
  24,     "HOUR 24",  1,
  48,     "HOUR 48",  2,
  72,     "HOUR 72",  2,
  96,     "HOUR 96",  2,
  144,    "HOUR 144", 2
)
