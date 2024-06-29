#' Serum creatinine estimation from eGFR (MDRD)
#'
#' Inverse of the function published in
#' \href{https://www.kidney.org/content/mdrd-study-equation}{National Kidney Foundation}
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encocded as number (female is 1) or character (female is "F").
#' @param race Race as per CDISC nomenclature. Black race is identified as the
#'   occurrence of 'black' in the value.
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_mdrd <- function(egfr, age, sex, race = "") {
  female_factor <- ifelse((sex <- 1) | (sex == "F"), .742, 1)
  race_factor <- ifelse(grepl("black", str_to_lower(race)), 1.212, 1)

  crea <- (egfr / (175 * age^-0.203 * female_factor * race_factor))^(1 / -1.154)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}


#' Serum creatinine estimation from eGFR (Raynaud method)
#'
#' Inverse of the function published in
#' \href{https://doi.org/10.1136/bmj-2022-073654}{Raynaud, BMJ 2023;381:e073654}
#'
#' To convert crea from mg/dl to umol/l, multiply by 88.4.
#'
#' @param egfr EGFR in ml/min/1.73 m^2.
#' @param age Age in years.
#' @param sex Sex encocded as number (male is 0) or character (male is "M").
#' @param race Race. Dummy variable for compatibility, race is not used by this
#'   method.
#'
#' @importFrom pracma lambertWn
#' @importFrom pracma lambertWp
#'
#' @return Serum creatinine in mg/dl.
#' @export
crea_raynaud <- function(egfr, age, sex, race = "") {
  male <- ifelse((sex == 0) | (sex == "M"), 1, 0)
  A <- 4.4275492 - 0.0055068 * age + 0.1806494 * male
  B <- 0.8230475
  C <- 0.0124264

  z <- 2 * exp(2 * A / B) * C * egfr^(-2 / B) / B
  # if(z < -1/exp(1)) {
  #   stop("z is < -1/e")
  # }


  # if(z < 0) {
  #   W <- pracma::lambertWn(z)
  # } else {
  W <- pracma::lambertWp(z)
  # }
  crea <- 0.707107 * sqrt(B) * sqrt(W) / sqrt(C)
  attr(crea, "unit") <- "mg/dl"
  return(crea)
}


#' Synthesize baseline serum creatinine for fictional subjects
#'
#' @details This function makes use of empirical eGFR data from a study in
#' non-diseased Caucasian subjects as published by [Wetzels et
#' al.](https://doi.org/10.1038/sj.ki.5002374), Table 1. In this study, eGFR
#' were calculated by the Modification of Diet in Renal Disease (MDRD) method.
#'
#' The tabulated data is modeled using a generalized linear model based on age
#' and sex. The model is used to predict the target eGFR (`target_egfr`) for the
#' subjects in DM. An actual eGFR (`egfr`) is then generated for each subjects
#' as a random sample from a normal distribution with the target eGFR as mean
#' and a standard deviation of 13 (i.e., the mean SD in the empirical data).
#'
#' A corresponding baseline creatinine concentration is then calculated from the
#' actual eGFR using the method specified in `crea_method`.
#'
#' @param dm The DM domain as data frame.
#' @param crea_method The creatinine calculation function as function reference.
#'   Can currently be `crea_mdrd` or `crea_raynaud`.
#' @importFrom stats glm
#' @importFrom stats predict
#' @return A DM domain with additional fields as data frame.
#' @keywords internal
synthesize_crea <- function(dm, crea_method = crea_mdrd) {
  empirical_egfr <- tribble(
    # Source: DOI:https://doi.org/10.1038/sj.ki.5002374, Table 1.
    ~female, ~age_lo, ~age_hi, ~N, ~Mean, ~SD, ~min, ~max, ~P5, ~P25, ~P50, ~P75, ~P95,
    0, 18, 24, 94, 100, 13, 72, 137, 77, 90, 99, 109, 121,
    0, 25, 29, 96, 93, 13, 67, 125, 74, 82, 90, 102, 117,
    0, 30, 34, 118, 86, 13, 63, 133, 68, 77, 85, 93, 107,
    0, 35, 39, 125, 85, 14, 61, 118, 65, 74, 85, 95, 110,
    0, 40, 44, 143, 84, 13, 54, 124, 66, 76, 83, 92, 106,
    0, 45, 49, 160, 83, 13, 50, 123, 63, 73, 82, 91, 105,
    0, 50, 54, 143, 79, 12, 46, 120, 60, 71, 78, 87, 97,
    0, 55, 59, 158, 76, 13, 27, 118, 58, 68, 75, 84, 98,
    0, 60, 64, 149, 75, 15, 48, 199, 59, 67, 73, 83, 95,
    0, 65, 69, 154, 75, 14, 51, 165, 56, 66, 74, 82, 97,
    0, 70, 74, 102, 71, 12, 38, 102, 54, 64, 70, 79, 92,
    0, 75, 79, 112, 70, 13, 41, 110, 45, 62, 70, 79, 91,
    0, 80, 84, 73, 67, 15, 41, 129, 43, 58, 69, 77, 87,
    0, 85, NA, 33, 62, 16, 34, 101, 35, 47, 65, 72, 92,
    1, 18, 24, 187, 91, 15, 58, 186, 72, 80, 90, 99, 112,
    1, 25, 29, 159, 85, 13, 55, 140, 63, 76, 83, 93, 107,
    1, 30, 34, 171, 85, 15, 53, 153, 63, 74, 83, 93, 113,
    1, 35, 39, 193, 79, 13, 55, 165, 63, 72, 76, 85, 102,
    1, 40, 44, 195, 77, 12, 48, 117, 58, 67, 77, 84, 100,
    1, 45, 49, 227, 74, 10, 47, 109, 56, 67, 74, 81, 91,
    1, 50, 54, 191, 73, 13, 51, 152, 56, 64, 71, 79, 93,
    1, 55, 59, 174, 70, 12, 48, 149, 53, 63, 69, 76, 89,
    1, 60, 64, 180, 68, 12, 41, 148, 50, 61, 68, 75, 84,
    1, 65, 69, 156, 66, 10, 44, 102, 52, 60, 65, 71, 85,
    1, 70, 74, 95, 66, 11, 40, 96, 49, 58, 64, 73, 85,
    1, 75, 79, 77, 62, 11, 37, 100, 45, 54, 61, 69, 82,
    1, 80, 84, 40, 64, 14, 46, 114, 46, 56, 62, 73, 88,
    1, 85, NA, 27, 59, 14, 30, 87, 36, 48, 61, 69, 78
  ) %>%
    mutate(AGE = age_lo + (age_hi - age_lo) / 2) %>%
    mutate(EGFR = Mean)

  m <- stats::glm(EGFR ~ AGE + female,
                  family = "gaussian",
                  data = empirical_egfr)
  dm <- dm %>%
    mutate(target_egfr = stats::predict(m, dm %>%
                                          mutate(female = case_when(SEX == "F" ~ 1, .default = 0))))
  renal <- rnorm(nrow(dm), dm$target_egfr, 13)
  dm %>%
    mutate(EGFR = renal) %>%
    mutate(CREA = crea_method(EGFR, AGE, SEX, RACE))
}


#' Synthesize baseline LB data
#'
#' @param dm The DM domain as data frame.
#' @return The LB domain as data frame.
#' @keywords internal
synthesize_lb <- function(dm) {
  dm %>%
    synthesize_crea() %>%
    select(c("STUDYID", "USUBJID", "DOMAIN", "RFSTDTC", "CREA")) %>%
    mutate(
      DOMAIN = "DM",
      LBSEQ = 1,
      LBCAT = "BIOCHEMISTRY",
      LBSPEC = "SERUM",
      VISITNUM = 1,
      LBBLFL = "Y",
      LBDTC = .data$RFSTDTC - 24 * 60 * 60,
      LBTESTCD = "CREAT",
      LBTEST = "Creatinine",
      LBORRES = .data$CREA,
      LBORRESU = "mg/dL",
      LBORNRLO = 0.67,
      LBORNRHI = 1.17,
      LBSTRESN = .data$LBORRES * 88.4,
      LBSTRESC = as.character(round(.data$LBSTRESN, 3)),
      LBSTRESU = "umol/L",
      LBSTNRLO = 59.2,
      LBSTNRHI = 103.4
    ) %>%
    select(-c("RFSTDTC", "CREA"))
}
