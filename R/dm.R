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
