

#' Create PC based on single-dose administration
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param vs The VS domain as data frame.
#' @param lb The LB domain as data frame.
#' @param sampling_scheme The PK sampling scheme as data frame.
#' @return The PC domain as data frame.
#' @keywords internal
make_sd_pc <- function(dm, ex, vs, lb, treatment_table, sampling_scheme) {
  sbs <- subject_baseline_data(dm, vs, lb) %>%
    recode_sex() %>%
    select(any_of(c("ID", "USUBJID", "SEX", "AGE", "HEIGHT", "WEIGHT", "EGFR"))) #%>%
    # left_join(treatment_table, by = "USUBJID")

  admin <- sbs %>%
    left_join(treatment_table, by = "USUBJID") %>%
    mutate(id = .data$ID, time = (.data$EXDY-1) * 24, cmt = "depot",
           amt = .data$EXDOSE, rate = -2, ii = 0, addl = 0, evid = 1, ss = 0,
           dur = 0, NTIME = .data$time) %>%
    select(any_of(c(
      "id", "time", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur",
      "NTIME", "USUBJID", "SEX", "AGE", "HEIGHT", "WEIGHT", "EGFR", "FOOD",
      "EXDOSFRM", "CL_FACTOR")))

  obs <- admin %>%
    mutate(evid = 0, cmt = "(obs)", rate = NA, ii = NA, addl = NA, ss = NA,
           dur = NA) %>%
    rowwise() %>%
    mutate(obs = list(sampling_scheme$time)) %>%
    tidyr::unnest(obs) %>%
    left_join(select(sampling_scheme, time, window), by = c("obs" = "time")) %>%
    mutate(NTIME = obs) %>%
    mutate(time = time + obs) %>%
    mutate(time = case_when(obs == 0 ~ time - runif(1) * window,
                            .default = time - window/2 + runif(1) * window)) %>%
    select(-c("window", "obs")) %>%
    as.data.frame() %>%
    mutate(FORM = case_match(EXDOSFRM, "TABLET" ~ 0, "CAPSULE" ~ 1))

  ev <- bind_rows(admin, obs) %>%
    arrange(USUBJID, time, -evid)

  sim <- pk_sim(ev) %>%
    left_join(distinct(sbs, ID, USUBJID), by = c("id" = "ID"))

  pc <- sim %>%
    select(c("USUBJID", "ID" = "id", "time", "NTIME", "c_centr", "c_metab")) %>%
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
    mutate(PCELTM = paste0("PT", as.character(.data$NTIME), "H")) %>%
    mutate(PCTPTNUM = .data$NTIME) %>%
    # left_join(sampling_scheme, by = "time") %>%
    mutate(PCDTC = .data$RFSTDTC + .data$time * 60 * 60) %>%
    arrange(.data$ID, .data$PCDTC, .data$PCTESTCD) %>%
    group_by(.data$ID) %>%
    mutate(PCSEQ = row_number()) %>%
    ungroup() %>%
    mutate(PCSPEC = "PLASMA") %>%
    mutate(PCRFTDTC = .data$RFSTDTC) %>%
    dplyr::select(-c("ID", "time", "c_centr", "c_metab", "RFSTDTC"))
  return(pc)
}
