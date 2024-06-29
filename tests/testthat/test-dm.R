test_that("synthesize_subjects works with default settings", {
  expect_no_error(
    synthesize_subjects()
  )
})

test_that("synthesize_dm works with default settings", {
  expect_no_error(
    temp <- synthesize_dm()
  )
  expect_equal(
    temp %>%
      filter(ACTARMCD != "SCRNFAIL") %>%
      nrow(),
    10)
})
