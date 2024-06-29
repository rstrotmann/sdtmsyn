#' Reformat date to SDTM conventions
#'
#' @param x The date in POSIX format
#' @return Date-time formatted as xxxx-xx-xxTxx:xx:xx
#' @keywords internal
reformat_date <- function(x) {
  return(format(x, "%Y-%m-%dT%H:%M"))
}

