# Load required libraries
library(quantmod)
library(ggplot2)

# Step 1: Fetch Data
stock_symbol <- readline(prompt = "Enter stock symbol: ")
market_symbol <- readline(prompt = "Enter market index symbol: ")
start_date <- readline(prompt = "Enter start date (YYYY-MM-DD): ")
end_date <- readline(prompt = "Enter end date (YYYY-MM-DD): ")
risk_free_rate <- as.numeric(readline(prompt = "Enter annual risk-free rate (in decimal, e.g., 0.02 for 2%): "))

# Get historical stock and market data
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
market_data <- getSymbols(market_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# Use adjusted closing prices
stock_prices <- Cl(stock_data)
market_prices <- Cl(market_data)

# Step 2: Calculate Returns
stock_returns <- dailyReturn(stock_prices)
market_returns <- dailyReturn(market_prices)

# Merge returns into a single dataset
returns_data <- merge(stock_returns, market_returns, all = FALSE)
colnames(returns_data) <- c("Stock", "Market")

# Step 3: Calculate Beta
model <- lm(Stock ~ Market, data = returns_data)
beta <- coef(model)["Market"]

# Print beta value
cat("Calculated Beta:", beta, "\n")

# Step 4: Apply CAPM Formula
risk_free_rate <- 0.02       # Example: 2% annual risk-free rate
expected_market_return <- 0.08  # Example: 8% annual expected market return
expected_stock_return <- risk_free_rate + beta * (expected_market_return - risk_free_rate)

cat("Expected Return of the Stock (CAPM):", round(expected_stock_return * 100, 2), "%\n")

# Step 5: Visualization
# Create a scatter plot of stock vs. market returns
ggplot(returns_data, aes(x = Market, y = Stock)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = paste("CAPM Analysis: Stock vs Market Returns for", stock_symbol),
    x = "Market Returns",
    y = "Stock Returns"
  ) +
  theme_minimal()
