# capm_calculator.R

# Constants 
RISK_FREE_RATE <- 0.02 # 2% risk-free rate 
MARKET_RETURN <- 0.08 # 8% expected market return

validate_inputs <- function(risk_free_rate, market_return, beta) { 
if (!is.numeric(risk_free_rate) || risk_free_rate < 0) { 
stop("Invalid risk-free rate. It should be a positive numeric value.") 
} 
if (!is.numeric(market_return) || market_return < 0) {
 stop("Invalid market return. It should be a positive numeric value.") 
} 
if (!is.numeric(beta) || beta <= 0) { 
stop("Invalid beta. It should be a positive numeric value.") 
} 
}

calculate_capm <- function(risk_free_rate, market_return, beta) {
validate_inputs(risk_free_rate, market_return, beta) 
# CAPM formula 
expected_return <- risk_free_rate + beta * (market_return - risk_free_rate) 

return(expected_return)
 }
