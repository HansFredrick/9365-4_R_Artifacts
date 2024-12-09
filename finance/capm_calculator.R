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
#' @param risk_free_rate Numeric value representing the risk-free rate (default 0.02). 
#' @param market_return Numeric value representing the expected market return. #' @param beta Numeric or vector of numeric values representing the stock's beta. 
#' @return A numeric value or vector of expected returns. 

calculate_capm <- function(risk_free_rate = RISK_FREE_RATE, market_return, beta, line_color = "blue") {
  validate_inputs(risk_free_rate, market_return, beta)
  
  calc_return <- map_dbl(beta, function(b) risk_free_rate + (b * (market_return - risk_free_rate)))
  
  df <- tibble(beta = beta, return = calc_return * 100)
  
  ggplot(df, aes(x = beta, y = return)) + 
    geom_line(color = line_color) + 
    ggtitle("CAPM: Expected Return vs. Beta") + 
    xlab("Beta") + 
    ylab("Expected Return (%)")
  
  return(calc_return * 100)
}

#' Validate Input Parameters for CAPM Calculation 
#' 
#' @param risk_free_rate Numeric value representing the risk-free rate. 
#' @param market_return Numeric value representing the market return. 
#' @param beta Numeric or vector of numeric values representing the stock's beta. 
#' @throws Error if any input is invalid. 

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
  if (!is.numeric(beta) || any(beta <= 0)) {
    stop("Error: Beta must be a positive numeric value or a vector of positive values.")
  }
}

test_that("CAPM function handles vector beta values", {
  result <- calculate_capm(0.02, 0.08, c(1.1, 1.2, 1.3))
  expect_equal(length(result), 3)
  expect_equal(result[1], 8.0)
})

test_that("CAPM visualization works", {
  result <- capture.output(calculate_capm(0.02, 0.08, c(1.1, 1.2, 1.3)))
  expect_true(any(grepl("Expected Return vs. Beta", result)))
})

test_that("CAPM function handles negative beta values", {
  expect_error(calculate_capm(0.02, 0.08, -1))
  expect_error(calculate_capm(0.02, 0.08, c(-1, 1.2)))
})
