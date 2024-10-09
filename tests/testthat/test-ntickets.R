library(testthat)
library(Math4753Hansen2024)

test_that("ntickets function returns correct values", {
  # Test case 1: Basic functionality
  result <- ntickets(100, 0.05, 0.9)
  expect_equal(result$N, 100)
  expect_equal(result$p, 0.9)
  expect_equal(result$gamma, 0.05)

  # Test case 2: Check if nd and nc are greater than or equal to N
  expect_true(result$nd >= 100)
  expect_true(result$nc >= 100)

  # Test case 3: Check if the probabilities are calculated correctly
  # Here we can check if the calculated probabilities are less than or equal to gamma
  prob_overbook_discrete <- 1 - pbinom(100, result$nd, 0.9)
  prob_overbook_normal <- 1 - pnorm(100 + 0.5, mean = result$nc * 0.9, sd = sqrt(result$nc * 0.9 * (1 - 0.9)))

  expect_true(prob_overbook_discrete <= 0.05)
  expect_true(prob_overbook_normal <= 0.05)
})

test_that("ntickets handles edge cases", {
  # Test case 4: Edge case with zero tickets
  result <- ntickets(0, 0.05, 0.9)
  expect_equal(result$N, 0)
  expect_equal(result$nd, 0)
  expect_equal(result$nc, 0)

  # Test case 5: Edge case with maximum probability
  result <- ntickets(100, 0.05, 1)
  expect_true(result$nd >= 100)
  expect_true(result$nc >= 100)
})

test_that("ntickets handles invalid inputs", {
  # Adjust the expected error message to match exactly
  expect_error(ntickets(100, 0.05, 1.5), "Invalid probability: p must be between 0 and 1")

  # Example of another potential invalid input test, with appropriate specific message
  expect_error(ntickets(-10, 0.05, 0.9), "Invalid input: N must be a non-negative number")
})
