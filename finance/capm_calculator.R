# capm_calculator.R

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

#' Calculate Expected Return using the CAPM Formula 
#' 
#' @param risk_free_rate Numeric value representing the risk-free rate (default 0.02). 
#' @param market_return Numeric value representing the expected market return. #' @param beta Numeric or vector of numeric values representing the stock's beta. 
#' @return A numeric value or vector of expected returns. 

calculate_capm <- function(risk_free_rate = RISK_FREE_RATE, market_return, beta) {
  validate_inputs(risk_free_rate, market_return, beta)
  
  print("Starting CAPM calculations for beta values...")
  
  calc_return <- vapply(beta, function(b) risk_free_rate + (b * (market_return - risk_free_rate)), numeric(1))
  
  print("CAPM calculations completed.")
  
  return(calc_return * 100)
}

#' Validate Input Parameters for CAPM Calculation 
#' 
#' @param risk_free_rate Numeric value representing the risk-free rate. 
#' @param market_return Numeric value representing the market return. 
#' @param beta Numeric or vector of numeric values representing the stock's beta. 
#' @throws Error if any input is invalid. 

validate_inputs <- function(risk_free_rate, market_return, beta) {
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

library(testthat)
