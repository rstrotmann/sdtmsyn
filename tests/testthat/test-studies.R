test_that("SAD study", {
  expect_no_error(
    make_study_sad()
  )
})

test_that("FE study", {
  expect_no_error(
    make_study_fe()
  )
})

test_that("rBA study", {
  expect_no_error(
    make_study_rba()
  )
})

test_that("DDI study", {
  expect_no_error(
    make_study_itz_rifa()
  )
})
