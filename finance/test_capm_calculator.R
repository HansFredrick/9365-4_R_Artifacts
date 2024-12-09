# test_capm_calculator.R

library(testthat)
library(purrr)
library(ggplot2)
source("capm_calculator.R")

# Test case to check CAPM function with vector beta values
test_that("CAPM function handles vector beta values", {
  result <- calculate_capm(0.02, 0.08, c(1.1, 1.2, 1.3))
  expect_equal(length(result), 3)
  expect_equal(result[1], 8.0)
})

# Test case to check visualization in CAPM
test_that("CAPM visualization works", {
  result <- capture.output(calculate_capm(0.02, 0.08, c(1.1, 1.2, 1.3)))
  expect_true(any(grepl("Expected Return vs. Beta", result)))
})

# Test case to handle negative beta values
test_that("CAPM function handles negative beta values", {
  expect_error(calculate_capm(0.02, 0.08, -1))
  expect_error(calculate_capm(0.02, 0.08, c(-1, 1.2)))
})

# Test case for invalid risk-free rate
test_that("CAPM function handles invalid risk-free rate", {
  expect_error(calculate_capm(-0.02, 0.08, c(1.1, 1.2)))
  expect_error(calculate_capm("invalid", 0.08, c(1.1, 1.2)))
})

# Test case for invalid market return
test_that("CAPM function handles invalid market return", {
  expect_error(calculate_capm(0.02, -0.08, c(1.1, 1.2)))
  expect_error(calculate_capm(0.02, "invalid", c(1.1, 1.2)))
})

# Test case for caching behavior
test_that("CAPM function uses cache for repeated inputs", {
  result_1 <- calculate_capm(0.02, 0.08, c(1.1, 1.2))
  result_2 <- calculate_capm(0.02, 0.08, c(1.1, 1.2))
  
  # Check if both results are identical (cached)
  expect_equal(result_1, result_2)
})

test_that("CAPM function does not cache when inputs change", {
  result_1 <- calculate_capm(0.02, 0.08, c(1.1, 1.2))
  result_2 <- calculate_capm(0.02, 0.09, c(1.1, 1.2))
  
  # Check if results are different for changed market return
  expect_false(identical(result_1, result_2))
})

# Test case for checking plotting behavior
test_that("CAPM plot is generated correctly", {
  plot_output <- capture.output(calculate_capm(0.02, 0.08, c(1.1, 1.2)))
  expect_true(any(grepl("Expected Return vs. Beta", plot_output)))
})
