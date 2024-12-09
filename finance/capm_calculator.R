# capm_calculator.R

library(ggplot2)
library(purrr)
library(testthat)
library(future.apply)

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

#' Calculate Expected Return using the CAPM Formula 
#' 
#' This function computes the expected return of a stock using the Capital Asset Pricing Model (CAPM). 
#' The formula is: expected_return = risk_free_rate + beta * (market_return - risk_free_rate).
#'
#' @param risk_free_rate Numeric value representing the risk-free rate (default is 0.02).
#' @param market_return Numeric value representing the expected market return. Can be a vector of values.
#' @param beta Numeric or vector of numeric values representing the stock's beta. A higher beta implies higher risk.
#' @param line_color A string indicating the color for the line in the plot (default is "blue").
#' 
#' @return A numeric vector of expected returns (multiplied by 100 to show percentage).
#' @details If the function has previously calculated the expected return for the same inputs, 
#' it will return the cached result to avoid redundant calculations.
#' 
#' @examples
#' calculate_capm(0.02, 0.08, c(1.1, 1.2, 1.3))
#' calculate_capm(0.02, 0.08, 1.5)
calculate_capm_cache <- list()

calculate_capm <- function(risk_free_rate = RISK_FREE_RATE, market_return, beta, line_color = "blue") {
  cache_key <- paste(risk_free_rate, market_return, paste(beta, collapse = ","), sep = "_")
  
  if (exists(cache_key, envir = calculate_capm_cache)) {
    cat("Fetching cached result...\n")
    return(get(cache_key, envir = calculate_capm_cache))
  }
  
  if (length(market_return) > 1) {
    calc_return <- mapply(function(r, b) risk_free_rate + (b * (r - risk_free_rate)), market_return, beta)
  } else {
    calc_return <- map_dbl(beta, function(b) risk_free_rate + (b * (market_return - risk_free_rate)))
  }
  
  df <- tibble(beta = rep(beta, length(market_return)), return = calc_return * 100)
  
  ggplot(df, aes(x = beta, y = return)) + 
    geom_line(color = line_color) + 
    ggtitle("CAPM: Expected Return vs. Beta") + 
    xlab("Beta") + 
    ylab("Expected Return (%)")
  
  assign(cache_key, calc_return, envir = calculate_capm_cache)
  
  return(calc_return * 100)
}

#' Validate Input Parameters for CAPM Calculation
#' 
#' This function checks the validity of the input parameters for the CAPM calculation.
#' It ensures that beta is a non-empty, positive numeric vector, and that both 
#' the risk-free rate and market return are non-negative numeric values.
#' 
#' @param risk_free_rate Numeric value representing the risk-free rate.
#' @param market_return Numeric value representing the market return.
#' @param beta Numeric or vector of numeric values representing the stock's beta.
#' 
#' @throws Error if any input is invalid (e.g., non-numeric values or negative numbers).
#' @examples
#' validate_inputs(0.02, 0.08, c(1.1, 1.2))
validate_inputs <- function(risk_free_rate, market_return, beta) {
  if (length(beta) == 0) {
    stop("Error: Beta cannot be an empty vector.")
  }
  if (!is.numeric(risk_free_rate) || risk_free_rate < 0) {
    stop("Error: Risk-free rate must be a non-negative numeric value.")
  }
  if (!is.numeric(market_return) || market_return < 0) {
    stop("Error: Market return must be a non-negative numeric value.")
  }
  if (!all(is.numeric(beta)) || any(beta <= 0)) {
    stop("Error: Beta must be a vector of positive numeric values.")
  }
}

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
