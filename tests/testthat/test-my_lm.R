test_that("my_lm works", {
  test_data <- my_gapminder
  test_formula <- lifeExp ~ gdpPercap + continent
  mylm_result <- my_lm(test_formula, test_data)
  lm_result <- lm(lifeExp ~ gdpPercap + continent, data =my_gapminder)
  mylm_coe <- round(mylm_result$Estimate, 3)
  lm_coe <- unname(lm_result$coefficients)
  lm_coe <- round(lm_coe, 3)
  expect_equal(mylm_coe, lm_coe)
})
