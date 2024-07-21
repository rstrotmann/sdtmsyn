
test_that("make_randomization works", {
  dm <- synthesize_dm(nsubs = 10)
  r <- randomization_table(dm)

  # all subjects have the same number of treatments
  expect_equal(
    r %>%
      group_by(USUBJID) %>%
      summarize(n = n()) %>%
      distinct(n) %>%
      nrow(),
    1)
})


test_that("randomization for single-dose", {
  dm <- synthesize_dm()

  r <- randomization_table(dm)
  expect_equal(
    dm %>%
      filter(ACTARMCD != "SCRNFAIL") %>%
      nrow(),
    nrow(r) /2
  )
})


test_that("ex for single dose", {
  dm <- synthesize_dm()

  r <- randomization_table(
    dm,
    sequence = data.frame(
      SEQUENCE = "A"
    ),
    trtdy = 1,
    treatment = data.frame(
      TREATMENT = c("A"))
  )
  expect_no_error(
    ex <- synthesize_sd_ex(dm, r))

  expect_equal(nrow(ex),
               dm %>%
                 filter(ACTARMCD != "SCRNFAIL") %>%
                 nrow())
})


test_that("ex for SAD", {
  dm <- synthesize_dm(nsub = 12)
  r <- sad_table(
    dm,
    treatment = tibble::tribble(
      ~EXDOSE, ~N,
      5,       6,
      10,      6)
  )
  expect_no_error(
    synthesize_sd_ex(dm, r))
})







