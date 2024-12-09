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

