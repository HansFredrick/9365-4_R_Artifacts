# capm_calculator.R

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

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

calculate_capm <- function(risk_free_rate = RISK_FREE_RATE, market_return, beta) {
validate_inputs(risk_free_rate, market_return, beta) 

print(paste("Processing CAPM calculation with risk-free rate =", risk_free_rate, ", market return =", market_return, ", and beta =", beta))

# CAPM formula 
# Check if beta is a vector 

if (length(beta) > 1) { 
calc_return <- sapply(beta, function(b) risk_free_rate + b * (market_return - risk_free_rate)) 
} else { 
calc_return <- risk_free_rate + beta * (market_return - risk_free_rate) 
}

# Convert to percentage 
calc_return <- calc_return * 100

return(calc_return)
 }
