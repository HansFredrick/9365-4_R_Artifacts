# capm_calculator.R

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

# Validation of inputs
validate_inputs <- function(risk_free_rate, market_return, beta) { 
if (!is.numeric(risk_free_rate) || risk_free_rate < 0) { 
stop("Error: Risk-free rate must be a non-negative numeric value.") 
} 
if (!is.numeric(market_return) || market_return < 0) { 
stop("Error: Market return must be a non-negative numeric value.") 
} 
if (!is.numeric(beta) || length(beta) < 1 || any(beta <= 0)) { stop("Error: Beta must be a positive numeric value or a vector of positive values.") 
}
}

# CAPM Calculation Function
calculate_capm <- function(risk_free_rate = RISK_FREE_RATE, market_return, beta) { 
validate_inputs(risk_free_rate, market_return, beta) 

calc_return <- risk_free_rate + (beta * (market_return - risk_free_rate))

return(calc_return * 100) 
}

library(testthat)
 
test_that("CAPM function calculates correctly", { expect_equal(calculate_capm(0.02, 0.08, 1.2), 8) expect_equal(calculate_capm(0.02, 0.08, c(1, 2)), c(8, 10)) 
})
