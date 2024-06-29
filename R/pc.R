

#' Create PC based on single-dose administration
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param vs The VS domain as data frame.
#' @param lb The LB domain as data frame.
#' @param sampling_scheme The PK sampling scheme as data frame.
#' @return The PC domain as data frame.
#' @keywords internal
make_sd_pc <- function(dm, ex, vs, lb, sampling_scheme) {
  sbs <- subject_baseline_data(dm, vs, lb) %>%
    left_join(ex %>%
                distinct(.data$USUBJID, .data$EXDOSE) %>%
                select(c("USUBJID", "EXDOSE")), by = "USUBJID") %>%
    mutate(FOOD = 0, PERIOD = 1)

  ev <- rxode2::et(amountUnits = "mg", timeUnits = "hours") %>%
    rxode2::add.dosing(
      dose = 500, dosing.to = "depot",
      rate = -2, start.time = 0
    ) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id = sbs$ID) %>%
    mutate(NTIME = .data$time) %>%
    left_join(
      sbs %>%
        dplyr::select(c("id" = "ID", "USUBJID", "SEX", "AGE", "HEIGHT",
                        "WEIGHT", "FOOD", "PERIOD", "EGFR", "EXDOSE")),
      by = "id") %>%
    mutate(amt = case_when(!is.na(.data$amt) ~ .data$EXDOSE, .default = NA)) %>%
    mutate(NTIME = .data$time)

  sim <- pk_sim(ev) %>%
    left_join(sbs %>% select(c("id" = "ID", "USUBJID")), by = "id")

  pc <- sim %>%
    dplyr::select(c("USUBJID", "id", "time", "c_centr", "c_metab")) %>%
    mutate(RS2023 = .data$c_centr * 1000, RS2023487A = .data$c_metab * 1000) %>%
    tidyr::pivot_longer(c("RS2023", "RS2023487A"),
                 names_to = "PCTESTCD",
                 values_to = "PCSTRESN"
    ) %>%
    mutate(PCTEST = case_when(
      .data$PCTESTCD == "RS2023" ~ "RS2023",
      .data$PCTESTCD == "RS2023487A" ~ "RS2023487A"
    )) %>%
    mutate(PCSTRESN = round(.data$PCSTRESN, 4)) %>%
    left_join(
      dm %>%
        distinct(.data$USUBJID, .data$RFSTDTC),
      by = "USUBJID"
    ) %>%
    mutate(STUDYID = unique(dm$STUDYID), DOMAIN = "PC") %>%
    mutate(PCELTM = paste0("PT", as.character(.data$time), "H")) %>%
    mutate(PCTPTNUM = .data$time) %>%
    left_join(sampling_scheme, by = "time") %>%
    mutate(PCDTC = .data$RFSTDTC + .data$time * 60 * 60) %>%
    arrange(.data$id, .data$PCDTC, .data$PCTESTCD) %>%
    group_by(.data$id) %>%
    mutate(PCSEQ = row_number()) %>%
    ungroup() %>%
    mutate(PCSPEC = "PLASMA") %>%
    mutate(PCRFTDTC = .data$RFSTDTC) %>%
    mutate(EPOCH = "OPEN LABEL TREATMENT") %>%
    dplyr::select(-c("id", "time", "c_centr", "c_metab", "RFSTDTC"))
  return(pc)
}
