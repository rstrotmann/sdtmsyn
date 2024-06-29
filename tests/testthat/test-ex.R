
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
  r <- randomization_table(
    dm,
    sequence = "A",
    adminday = 1,
    treatment = data.frame(TREATMENT = "A")
  )
  expect_equal(
    dm %>%
      filter(ACTARMCD != "SCRNFAIL") %>%
      nrow(),
    nrow(r)
  )
})


test_that("ex for single dose", {
  dm <- synthesize_dm()
  r <- randomization_table(
    dm,
    sequence = "A",
    adminday = 1,
    treatment = data.frame(
      TREATMENT = c("A"))
  )
  synthesize_sd_ex(dm, r)
})


test_that("ex for SAD", {
  dm <- synthesize_dm(nsub = 12)
  r <- randomization_table(
    dm,
    sequence = c("A", "B", "C", "D"),
    adminday = c(1),
    treatment = data.frame(
      TREATMENT = c("A", "B", "C", "D"),
      DOSE = c(50, 100, 200, 400))
  )
  synthesize_sd_ex(dm, r)
})



test_that("synthesize_sd_ex works", {
  dm <- synthesize_dm()
  r <- randomization_table(
    dm,
    treatment = data.frame(
      TREATMENT = c("A", "B"),
      DOSE = c(500, 250),
      EXDOSFRM = c("TABLET", "CAPSULE"))
  )
  synthesize_sd_ex(dm, r)
})


test_that("complex crossover", {
  dm <- synthesize_dm(nsubs = 12)
  r <- randomization_table(
    dm,
    sequence = c("ABC", "BAC"),
    adminday = c(1, 8, 15),
    treatment = data.frame(
      TREATMENT = c("A", "B", "C"),
      FASTED = c(0, 1, 1),
      PPI = c(0, 0, 1)
    )
  )
  synthesize_sd_ex(dm, r)
})


test_that("SAD", {
  dm <- synthesize_dm(nsubs = 15)
  r <- sad_table(dm)
  synthesize_sd_ex(dm, r)
})







