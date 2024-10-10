library(usethis)

sdtm_data_sad <- make_study_sad()
usethis::use_data(sdtm_data_sad, overwrite = TRUE)

sdtm_data_fe <- make_study_fe()
usethis::use_data(sdtm_data_fe, overwrite = TRUE)

sdtm_data_rba <- make_study_rba()
usethis::use_data(sdtm_data_rba, overwrite = TRUE)

sdtm_data_ddi <- make_study_itz_rifa()
usethis::use_data(sdtm_data_ddi, overwrite = TRUE)

sdtm_data_ri <- make_study_ri()
usethis::use_data(sdtm_data_ri, overwrite = TRUE)
