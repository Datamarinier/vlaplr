test_that("Function get right monday", {

  expect_identical(get_start_week("2021-11-03"), lubridate::date("2021-11-01"))

})
