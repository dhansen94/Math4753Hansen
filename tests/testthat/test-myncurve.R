library(testthat)
library(Math4753Hansen2024)

test_that("mean is correct", {
  result <- myncurve(10, 5, 6)
  expect_equal(result$mu, 10)
})

test_that("Sigma is correct", {
  result <- myncurve(10, 5, 6)
  expect_equal(result$sigma, 5)
})

test_that("Probability calculation is correct", {
  result <- myncurve(10, 5, 6)  # Ensure you are using the same variable
  expected_probability <- pnorm(6, mean = 10, sd = 5)


  expect_equal(result$probability, expected_probability)
})
