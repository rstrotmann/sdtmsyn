#' Create randomization table
#'
#' @param dm The DM domain as data frame
#' @param sequence The sequences as vector of character strings where A, B, C,
#' etc. identify the treatment codes.
#' @param adminday A vector of the treatment days as numeric.
#' @param treatment A data frame with TREATMENT giving the treatment codes and
#' further columns specifying the respective conditions in the treatment
#' periods.
#'
#' @return The randomization scheme as data frame.
#' @export
randomization_table <- function(
    dm,
    sequence = c("AB", "BA"),
    adminday = c(1, 8),
    treatment = data.frame(
      TREATMENT = c("A", "B"),
      DOSE = c(500, 250))
) {
  temp <- dm %>%
    group_by(ACTARMCD) %>%
    mutate(subject = case_when(ACTARMCD != "SCRNFAIL" ~ row_number())) %>%
    ungroup() %>%
    select(subject, USUBJID)

  nsubj <- max(temp$subject, na.rm = TRUE)
  nperiod <- unique(sapply(sequence, str_length))

  if(length(adminday) != nperiod)
    stop("Mismatch between number of administration days and periods")

  RL4(nsubj = nsubj, sequence)$rl %>%
    mutate(TREATMENT = strsplit(sequence, split = "", fixed = TRUE)) %>%
    unnest(TREATMENT) %>%
    group_by(subject) %>%
    mutate(PERIOD = row_number()) %>%
    mutate(EXDY = adminday[.data$PERIOD]) %>%
    left_join(treatment, by = "TREATMENT") %>%
    left_join(temp, by = "subject")
}


#' Make a treatment table for SAD study
#'
#' @param dm The DM domain as data frame.
#' @param treatment The treatments as data frame.
#'
#' @return A data frame.
#' @export
sad_table <- function(
    dm,
    treatment = data.frame(
      EXDOSE = c(50, 100, 200, 300),
      N = c(3, 6, 3, 3))
    ) {

  temp <- dm %>%
    filter(ACTARMCD != "SCRNFAIL") %>%
    mutate(subject = row_number()) %>%
    select(subject, USUBJID)

  nsubj <- max(temp$subject, na.rm = TRUE)

  temp %>%
    bind_cols(
      treatment %>%
        rowwise() %>%
        mutate(x = list(seq(1, N))) %>%
        unnest(x) %>%
        select(-c(N, x))
    ) %>%
    mutate(seqno = 1,
           sequence = "A",
           PERIOD = 1,
           TREATMENT = "A",
           EXDY = 1)
}


# ' Synthesize a fictional EX domain for single dose administration
# '
# ' @param dm The DM including the subject info for whom to synthesize EX.
# ' @param adminday The treatment days as vector.
# ' @param drug The name of the drug to be administered.
# ' @param route The administration route as character.
# ' @param form The dosage form as character.
# ' @param epoch The epoch label as character.
# ' @param dose The dose as numeric.
# '
# ' @return EX data as data frame.
# ' @keywords internal
# synthesize_sd_ex_old <- function(dm,
#                        adminday = 1,
#                        drug = "RS2023",
#                        route = "ORAL",
#                        form = "TABLET",
#                        dose = 500,
#                        epoch = "OPEN LABEL TREATMENT") {
#   dm %>%
#     dplyr::filter(.data$ACTARMCD != "SCRNFAIL") %>%
#     dplyr::select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
#     dplyr::mutate(DOMAIN = "EX") %>%
#     dplyr::group_by_all() %>%
#     tidyr::expand(EXDY = adminday) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#       EXSTDTC = .data$RFSTDTC + (.data$EXDY - 1) * 60 * 60 * 24,
#       EXSTDY = .data$EXDY,
#       EXENDY = .data$EXDY,
#       EXENDTC = .data$EXSTDTC,
#       EXTRT = drug,
#       EXDOSE = dose,
#       EXROUTE = route,
#       EXDOSFRM = form) %>%
#     dplyr::arrange(.data$USUBJID, .data$EXSTDTC) %>%
#     dplyr::group_by(.data$USUBJID) %>%
#     dplyr::mutate(EXSEQ = dplyr::row_number()) %>%
#     dplyr::ungroup() %>%
#     {if(length(adminday) == 1) dplyr::mutate(., EPOCH = epoch) else
#       dplyr::mutate(., EPOCH = paste(epoch, EXSEQ))} %>%
#     dplyr::select(-"RFSTDTC") %>%
#     as.data.frame()
# }


#' Create EX domain for single-dose studies
#'
#' @param dm The DM domain as data frame.
#' @param randomization The randomization table as data frame.
#' @param adminday The administration day(s) as numeric.
#' @param drug The drug as character.
#' @param route The administration route as character.
#' @param form The dosage form as character.
#' @param dose The dose as numeric.
#'
#' @return A data frame.
#' @export
synthesize_sd_ex <- function(
    dm,
    randomization,
    adminday = 1,
    drug = "RS2023",
    route = "ORAL",
    form = "TABLET",
    dose = 500) {
  ex <- dm %>%
    filter(ACTARMCD != "SCRNFAIL") %>%
    select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    mutate(DOMAIN = "EX") %>%
    mutate(
      EXTRT = drug,
      EXDOSE = dose,
      EXROUTE = route,
      EXDOSFRM = form) %>%
    select(-any_of(setdiff(names(randomization), "USUBJID"))) %>%
    left_join(randomization, by = "USUBJID") %>%
    {if(length(unique(randomization$TREATMENT)) == 1)
      mutate(., EPOCH = "OPEN LABEL TREATMENT") else
      mutate(., EPOCH = paste0("TREATMENT PERIOD ", PERIOD))} %>%
    mutate(EXSEQ = PERIOD) %>%
    select(-c("seqno", "sequence", "subject", "TREATMENT", "PERIOD")) %>%
    mutate(EXSTDTC = .data$RFSTDTC + (.data$EXDY - 1) * 60 * 60 * 24) %>%
    mutate(EXENDTC = .data$EXSTDTC) %>%
    as.data.frame()
  return(ex)
}










#' Create administration schedule with randomly missed doses
#'
#' @param start_dtc Starting DTC.
#' @param missed_prob Probability of missing administration.
#' @param end_dtc End DTC.
#' @param dose The normal dose.
#' @param dose_red The reduced dose.
#' @param red_prob The probability that a subject has a dose reduction. Dose
#'   reductions, if any, occur at a random day after day 7.
#' @return EXSTDTC and EXENDTC as data frame.
#' @keywords internal
miss_admins <- function(start_dtc, end_dtc, dose = 500, dose_red = 250,
                        missed_prob = 0.15, red_prob = 0.3) {
  # create missed administration days
  dose_reduction <- red_prob != 0
  treatment_duration <- as.numeric(end_dtc - start_dtc) + 1
  n <- floor(treatment_duration * runif(1, 0, missed_prob))
  admins <- data.frame(
    day = seq(1, treatment_duration),
    dtc = seq(start_dtc, end_dtc, by = "1 day"),
    dose = dose
  )
  missed_days <- sort(unique(floor(runif(n, 2, treatment_duration))))
  admins[missed_days, "dtc"] <- NA

  if (dose_reduction) {
    if (runif(1, 0, 1) < red_prob) {
      red_start_dy <- floor(runif(1, 7, treatment_duration))
      red_days <- seq(red_start_dy, treatment_duration)
      admins[red_days, "dose"] <- dose_red
    }
  }

  # to do:
  # change times slightly
  # omit times occasionally

  admins %>%
    mutate(prev_dose = lag(dose)) %>%
    mutate(dose_red_start = dose != prev_dose) %>%
    mutate(dose_restart = lag(is.na(dtc))) %>%
    filter(!is.na(dtc)) %>%
    mutate(block = dose_red_start == T | dose_restart == T | row_number() == 1) %>%
    group_by(block) %>%
    mutate(block_id = case_when(block == 1 ~ row_number(),
                                .default = NA
    )) %>%
    ungroup() %>%
    as.data.frame() %>%
    fill(block_id, .direction = "down") %>%
    group_by(block_id) %>%
    mutate(EXSTDTC = dtc[1], EXENDTC = dtc[n()]) %>%
    ungroup() %>%
    as.data.frame() %>%
    distinct(EXSTDTC, EXENDTC, DOSE = dose)
}



#' Synthesize a fictional EX domain for single dose administration
#'
#' @param dm The DM including the subject info for whom to synthesize EX.
#' @param drug The name of the drug to be administered.
#' @param dose The dose.
#' @param treatment_duration The treatment duration in days.
#' @param missed_prob Probability to miss doses.
#' @param red_prob The dose reduction probability.
#' @param missed_doses Switch whether to include randomly missed doses as boolean.
#'
#' @return The EX domain as data frame.
#' @keywords internal
synthesize_md_ex <- function(dm,
                       drug = "RS2023",
                       route = "ORAL",
                       form = "TABLET",
                       dose = 500,
                       epoch = "OPEN LABEL TREATMENT",
                       treatment_duration = 50,
                       missed_prob = 0.15,
                       missed_doses = T,
                       red_prob = 0.3){
  ex <- dm %>%
    dplyr::filter(.data$ACTARMCD != "SCRNFAIL") %>%
    dplyr::select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    dplyr::mutate(DOMAIN = "EX") %>%

    # make random treatment duration between 60% and 100% of the specified
    dplyr::mutate(trtdur = floor(runif(
      nrow(.), treatment_duration * .6,
      treatment_duration + 1))) %>%
    dplyr::mutate(
      EXSTDTC = .data$RFSTDTC,
      EXENDTC = .data$RFSTDTC + .data$trtdur * 3600 * 24
    )

  # randomly introduce missed doses and dose reductions
  if (missed_doses == TRUE) {
    ex <- ex %>%
      dplyr::group_by(.data$DOMAIN, .data$STUDYID, .data$USUBJID) %>%
      tidyr::expand(miss_admins(EXSTDTC, EXENDTC, red_prob = red_prob)) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }

  ex <- ex %>%
    dplyr::mutate(
      EXTRT = drug,
      EXDOSE = .data$DOSE,
      EPOCH = epoch,
      EXROUTE = route,
      EXDOSFRM = form
    ) %>%
    dplyr::arrange(.data$USUBJID, .data$EXSTDTC) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(EXSEQ = row_number()) %>%
    dplyr::ungroup()

  return(ex)
}
