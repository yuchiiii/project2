test_that("Two-sided t-test works", {
  x <- rnorm(10, 0, 1)
  result <- t.test(x, mu = 0)
  result1 <- my_t.test(x, "two.sided", 0)
  expect_equal(result1$p_val, result$p.value)
})

test_that("One-sided t-test works", {
  x <- rnorm(10, 0, 1)
  result <- t.test(x, alternative = "less", mu = 1)
  result1 <- my_t.test(x, "less", 1)
  expect_equal(result1$p_val, result$p.value)
})
