# capm_calculator.R

library(ggplot2)
library(purrr)
library(future.apply)

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

#' Calculate Expected Return using the CAPM Formula 
#' 
#' @param risk_free_rate Numeric value representing the risk-free rate (default 0.02). 
#' @param market_return Numeric value representing the expected market return. 
#' @param beta Numeric or vector of numeric values representing the stock's beta. 
#' @param line_color Character value for the color of the line in the plot (default "blue"). 
#' @return A numeric value or vector of expected returns. 
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
  if (!all(is.numeric(beta)) || any(beta <= 0)) {
    stop("Error: Beta must be a vector of positive numeric values.")
  }
}

