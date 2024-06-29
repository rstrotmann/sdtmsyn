#' Synthesize fictional baseline vital sign data (VS domain)
#'
#' @param dm The DM domain to provide the subject data.
#' @return The VS domain data as data frame.
#' @keywords internal
synthesize_vs <- function(dm) {
  dm %>%
    # filter(ACTARMCD!="SCRNFAIL") %>%
    dplyr::select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    group_by_all() %>%
    expand(VSTESTCD = c("HEIGHT", "WEIGHT")) %>%
    mutate(VSORRES = case_when(
      .data$VSTESTCD == "HEIGHT" ~ rnorm(1, 179, 6.81),
      .data$VSTESTCD == "WEIGHT" ~ rnorm(1, 78.5, 10.2)
    )) %>%
    mutate(VSTEST = case_when(
      .data$VSTESTCD == "HEIGHT" ~ "Height",
      .data$VSTESTCD == "WEIGHT" ~ "Weight"
    )) %>%
    mutate(VSORRESU = case_when(
      .data$VSTESTCD == "HEIGHT" ~ "cm",
      .data$VSTESTCD == "WEIGHT" ~ "kg"
    )) %>%
    mutate(VSSTRESN = round(.data$VSORRES, digits = 1)) %>%
    mutate(VSSTRESU = .data$VSORRESU, EPOCH = "SCREENING", DOMAIN = "VS",
           VSBLFL = "") %>%
    ungroup() %>%
    mutate(VISIT = "SCREENING") %>%
    mutate(VSBLFL = "Y") %>%
    mutate(VSDTC = .data$RFSTDTC) %>%
    select(-c("RFSTDTC")) %>%
    as.data.frame()
}
