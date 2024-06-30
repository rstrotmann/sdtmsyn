#' Synthesize SDTM data for a clinical study
#'
#' @param dm The DM domain as data frame.
#' @param treatment_table The treatment/randomization table as data frame.
#' @param sampling_scheme The sampling scheme as data frame.
#'
#' @return A list.
#' @export
make_study <- function(dm, treatment_table, sampling_scheme) {
  ex <- synthesize_sd_ex(dm, treatment_table)
  vs <- synthesize_vs(dm)
  lb <- synthesize_lb(dm)
  pc <- make_sd_pc(dm, ex, vs, lb, sampling_scheme)

  dm <- left_join(dm,
                  ex %>%
                    group_by(.data$USUBJID) %>%
                    mutate(RFENDTC = max(.data$EXENDTC, na.rm = T)) %>%
                    distinct(.data$USUBJID, .data$RFENDTC),
                  by = "USUBJID"
  )

  out = lapply(
    list(
      dm = dm,
      vs = vs,
      lb = lb,
      ex = ex,
      pc = pc
    ), isofy_dates)

  return(out)
}


#' Synthesize SDTM data for a SAD study
#'
#' @return A list of SDTM domains.
#' @export
make_study_sad <- function() {
  dm <- synthesize_dm(
    nsubs = 48,
    studyid = "2024000001",
    nsites = 1,
    female_fraction = 0,
    duration = 10,
    min_age = 18,
    max_age = 55)

  treatment_table <- sad_table(dm,
    treatment = tibble::tribble(
      ~EXDOSE, ~N,
      5,       3,
      10,      3 ,
      20,      3,
      50,      3,
      100,     6,
      200,     3,
      500,     6,
      800,     6,
      1000,    3,
      500,     12))
  make_study(dm, treatment_table, rich_sampling_scheme)
}


make_study_fe <- function() {
  dm <- synthesize_dm(
    nsubs = 16,
    studyid = "202400002",
    nsites = 1,
    female_fraction = 0.5,
    duration = 14)

  treatment_table <- randomization_table(
    dm,
    sequence = c("AB", "BA"),
    adminday = c(1, 8),
    treatment = data.frame(
      TREATMENT = c("A", "B"),
      FOOD = c(1, 0)
    ))
  make_study(dm, treatment_table, rich_sampling_scheme)
}



