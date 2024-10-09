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
  pc <- make_sd_pc(dm, ex, vs, lb, treatment_table, sampling_scheme)

  dm <- dm %>%
    left_join(
      ex %>%
        group_by(.data$USUBJID) %>%
        mutate(RFENDTC = max(.data$EXENDTC, na.rm = T)) %>%
        distinct(.data$USUBJID, .data$RFENDTC),
      by = "USUBJID") %>%

    left_join(
      treatment_table %>%
        distinct(USUBJID, temp_arm = ARM, temp_armcd = ARMCD),
      by = "USUBJID") %>%
    mutate(ARM = case_when(ARM == "" ~ temp_arm, .default = ARM)) %>%
    mutate(ARMCD = case_when(ARMCD == "" ~ temp_armcd, .default = ARMCD)) %>%
    select(-c("temp_arm", "temp_armcd")) %>%
    mutate(ARCTARM = ARM, ACTARMCD = ARMCD)

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


#' Synthesize SDTM data for a food effect study
#'
#' @return A list of SDTM domains.
#' @export
make_study_fe <- function() {
  dm <- synthesize_dm(
    nsubs = 16,
    studyid = "202400002",
    nsites = 1,
    female_fraction = 0.5,
    duration = 14)

  treatment_table <- randomization_table(
    dm,
    trtdy = c(1, 8),
    sequence = data.frame(
      SEQUENCE = c("AB", "BA"),
      ARMCD = c("AB", "BA"),
      ARM = c("Fed administration - Fasted administration",
              "Fasted administration - Fed administration")),
    treatment = data.frame(
      TREATMENT = c("A", "B"),
      FOOD = c(1, 0)))
  make_study(dm, treatment_table, rich_sampling_scheme)
}


#' Synthesize SDTM data for a relative BA study
#'
#' @return A list of SDTM domains.
#' @export
make_study_rba <- function() {
  dm <- synthesize_dm(
    nsubs = 16,
    studyid = "202400003",
    nsites = 1,
    female_fraction = 0,
    duration = 14)

  treatment_table <- randomization_table(
    dm,
    trtdy = c(1, 8),
    sequence = data.frame(
      SEQUENCE = c("AB", "BA"),
      ARMCD = c("AB", "BA"),
      ARM = c("Tablet - Capsule",
              "Capsule - Tablet")),
    treatment = data.frame(
      TREATMENT = c("A", "B"),
      EXDOSFRM = c("TABLET", "CAPSULE")))
  make_study(dm, treatment_table, rich_sampling_scheme)
}


#' Synthesize SDTM data for a DDI study with rifampin and itraconazole
#'
#' @return A list of SDTM domains.
#' @export
make_study_itz_rifa <- function() {
  dm <- synthesize_dm(
    nsubs = 16,
    studyid = "202400004",
    nsites = 1,
    female_fraction = 0,
    duration = 14)

  treatment_table <- randomization_table(
    dm,
    trtdy = c(1, 8, 22),
    sequence = data.frame(
      SEQUENCE = c("ABC"),
      ARMCD = c("ABC"),
      ARM = c("control - itraconazole - rifampicin")),
    treatment = data.frame(
      TREATMENT = c("A", "B", "C"),
      CL_FACTOR = c(1, 0.1, 10)
      )
    )

  make_study(dm, treatment_table, sampling_scheme = rich_sampling_scheme)
}


#' Synthesize SDTM data for a renal impairment study
#'
#' @return A list of SDTM domains.
#' @export
make_study_ri <- function() {
  dm <- synthesize_dm(
    nsubs = 36,
    studyid = "202400004",
    nsites = 1,
    female_fraction = 0.5,
    duration = 7)

  treatment_table <- randomization_table(
    dm,
    trtdy = c(1),
    sequence = data.frame(
      SEQUENCE = c("A", "B", "C"),
      ARMCD = c("normal", "mild", "moderate"),
      ARM = c("normal", "mild", "moderate")),
    treatment = data.frame(
      TREATMENT = c("A", "B", "C"),
      target_egfr = c(120, 55, 35)
    )
  )

  dm <- left_join(
    dm, select(treatment_table, "USUBJID", "target_egfr"), by = "USUBJID"
  )

  make_study(dm, treatment_table, rich_sampling_scheme)
}
