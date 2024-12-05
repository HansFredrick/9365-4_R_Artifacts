# Function to calculate
calculate_capm <- function(risk_free_rate, market_return, beta) {
  # CAPM formula
  expected_return <- risk_free_rate + beta * (market_return - risk_free_rate)
  
  # Return the result
  return(expected_return)
}

# Input values
risk_free_rate <- 0.02   # Example: 2% risk-free rate
market_return <- 0.08    # Example: 8% expected market return
beta <- 1.2              # Example: Beta of the stock

# Calculate expected return
expected_return <- calculate_capm(risk_free_rate, market_return, beta)

# Print the result
cat("The expected return of the stock is:", round(expected_return * 100, 2), "%\n")

