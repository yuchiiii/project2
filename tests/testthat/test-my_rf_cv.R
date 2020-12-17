test_that("my_rf_cv works", {
  result <- my_rf_cv(5)
  expect_type(result, "double")
})
