test_that("synthesize_crea", {
  # dm <- synthesize_dm(nsubs = 20)
  expect_no_error(
    crea <- synthesize_crea(synthesize_dm(nsubs = 20))
  )
  crea %>%
    ggplot2::ggplot(ggplot2::aes(x = ))

})


dm <- synthesize_dm(nsubs = 200)
vs <- synthesize_vs(dm) %>%
  mutate(target_egfr = 90)

temp <- vs  %>%
  left_join(dm, by = "USUBJID") %>%
  mutate(actual_egfr = rnorm(nrow(vs), vs$target_egfr, 13)) %>%
  mutate(CREA = crea_mdrd(actual_egfr, AGE, SEX, RACE))


temp %>%
  ggplot(aes(x = actual_egfr)) +
  geom_histogram(position = "identity", bins = 15) +
  geom_vline(xintercept = c(30, 60, 90), color = "red") +
  theme_bw()

temp %>%
  ggplot(aes(x = AGE, y = CREA, color = SEX)) +
  geom_point() +
  theme_bw()
