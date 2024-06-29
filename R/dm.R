#' Simulate fictional subject disposition data
#'
#' This function generates a pre-specified number of subjects across different
#'   clinical sites with fictional dates for signing the informed consent and
#'   treatment start.
#'
#' @param studyid The study identifier as string.
#' @param nsubs The number of subjects to be simulated.
#' @param nsites The number of clinical sites to be simulated.
#' @param screenfailure_rate The fraction of subjects to be screeing failures.
#' @param start_date The fictional study start date.
#'
#' @import lubridate
#' @return The disposition data for the simulated subjects as data frame.
#' @keywords internal
synthesize_subjects <- function(studyid = "2023001", nsubs = 10, nsites = 4,
                                screenfailure_rate = 0.25,
                                start_date = "2000-12-20 10:00") {
  current_date <- lubridate::as_datetime(start_date, format = "%Y-%m-%d %H:%M")
  site_names <- 100 + seq(1:nsites)
  sbs <- data.frame(
    SITEID = "", SUBJID = "", ACTARM = "", ACTARMCD = "", RFICDTC = NA,
    RFSTDTC = NA, RFXSTDTC = NA
  )
  sbs_by_site <- rep(0, nsites)

  while(
    nrow(filter(sbs, .data$ACTARMCD != "SCRNFAIL")) < nsubs + 1) {
    current_site <- round(runif(1, 1, nsites), 0)
    current_sb_no <- sbs_by_site[current_site] + 1
    current_date <- current_date + floor(abs(rnorm(1, 0.5, 2))) * 60 * 60 * 24
    siteid <- as.character(site_names[current_site])
    enrolled <- runif(1) > screenfailure_rate
    sbs_by_site[current_site] <- sbs_by_site[current_site] + 1
    sbs <- add_row(sbs,
      SITEID = siteid,
      SUBJID = paste0(
        siteid,
        as.character(formatC(current_sb_no, width = 4, flag = "0"))),
      ACTARMCD = case_when(!enrolled ~ "SCRNFAIL", .default = ""),
      ACTARM = case_when(!enrolled ~ "Screen Failure", .default = ""),
      RFICDTC = current_date + rnorm(1, 0, 1) * 60 * 60,
      RFSTDTC = case_when(
      enrolled ~ RFICDTC + floor(rnorm(1, 10, 2)) * 60 * 60 * 24,
      .default = NA
      ),
      RFXSTDTC = RFSTDTC
    )
  }
  return(sbs[-1, ])
}


#' Synthesize a fictional DM domain
#'
#' Currently geared toward a food effect study, but can be adapted easily to
#'   generate other study types.
#'
#' @param studyid The study ID.
#' @param nsubs The number of subjects.
#' @param nsites The number of clinical sites.
#' @param duration The duration of the study in days.
#' @param female_fraction The fraction of female subjects (between 0 and 1).
#' @param min_age The minimum age.
#' @param max_age The maximum age.
#' @return The DM data as data frame.
#' @import dplyr
#' @keywords internal
synthesize_dm <- function(studyid = "2023001", nsubs = 10, nsites = 5,
                          duration = 7, female_fraction = 0.5, min_age = 18,
                          max_age = 55) {
  sbs <- synthesize_subjects(studyid, nsubs, nsites)
  dm <- sbs %>%
    group_by_all() %>%
    mutate(
      STUDYID = as.character(studyid),
      USUBJID = paste0(.data$STUDYID, .data$SUBJID),
      SEX = case_when(runif(1) > female_fraction ~ "M", .default = "F"),
      AGE = round(runif(1, min_age, max_age), 0),
      AGEU = "YEARS",
      COUNTRY = "DEU",
      DOMAIN = "DM",
      ARM = .data$ACTARM,
      ARMCD = .data$ACTARMCD,
      RACE = cut(runif(1),
                 breaks = c(0, .05, .2, 1),
                 labels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
      ),
      ETHNIC = ""
    ) %>%
    as.data.frame()
  return(dm)
}
