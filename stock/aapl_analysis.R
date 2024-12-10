# Install and load required packages
required_packages <- c("quantmod", "ggplot2", "dplyr", "lubridate", "testthat")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
lapply(required_packages, library, character.only = TRUE)

# Function to fetch stock data
# Retrieves stock data from Yahoo Finance for a specified symbol and date range
fetch_stock_data <- function(symbol, start_date, end_date) {
  stopifnot(!missing(symbol), !missing(start_date), !missing(end_date))
  stock_data <- tryCatch(
    getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) stop("Error fetching stock data: ", e)
  )
  return(stock_data)
}

# Validate stock data (not NULL or empty)
validate_stock_data <- function(stock_data) {
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    stop("Error: Stock data must not be NULL or empty.")
  }
}

# Validate required columns in a data frame
validate_columns <- function(data, required_cols) {
  if (!all(required_cols %in% colnames(data))) {
    stop("Error: Missing required columns.")
  }
}

# Function to calculate quarters and years
calculate_quarters <- function(date_column) {
  data.frame(
    Quarter = paste0("Q", ceiling(as.numeric(format(date_column, "%m")) / 3)),
    Year = format(date_column, "%Y")
  )
}

# Process stock data: Aggregates stock data to calculate average closing prices by year and quarter
process_stock_data <- function(stock_data, close_column = "Close") {
  validate_stock_data(stock_data)
  stopifnot(close_column %in% colnames(stock_data))
  
  stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))
  stock_df <- cbind(stock_df, calculate_quarters(stock_df$Date))
  
  processed <- stock_df %>%
    group_by(Year, Quarter) %>%
    summarise(AvgClose = mean(.data[[close_column]], na.rm = TRUE), .groups = "drop")
  return(processed)
}

# Simulate search popularity data
simulate_search_popularity <- function(years, quarters, min_pop = 50, max_pop = 100) {
  stopifnot(length(years) > 0, length(quarters) > 0)
  expand.grid(Year = years, Quarter = quarters) %>%
    mutate(Popularity = round(runif(n(), min_pop, max_pop)))
}

# Merge processed stock data with search popularity data
merge_stock_and_search <- function(stock_data, search_data) {
  validate_columns(stock_data, c("Year", "Quarter"))
  validate_columns(search_data, c("Year", "Quarter"))
  merged <- merge(stock_data, search_data, by = c("Year", "Quarter"))
  return(merged)
}

# Calculate growth metrics for stock and popularity
calculate_growth <- function(data) {
  data %>%
    arrange(Year, Quarter) %>%
    mutate(
      StockGrowth = AvgClose / lag(AvgClose) - 1,
      PopularityGrowth = Popularity / lag(Popularity) - 1
    )
}

# Plot stock data vs. popularity
plot_data <- function(data, title = "Stock Data vs Popularity") {
  ggplot(data, aes(x = AvgClose, y = Popularity)) +
    geom_point() +
    labs(title = title, x = "Average Stock Closing Price", y = "Search Popularity") +
    theme_minimal()
}

# Create a comparison graph for stock data and popularity
create_comparison_graph <- function(data, scale_factor = 1) {
  ggplot(data, aes(x = Quarter)) +
    geom_bar(aes(y = AvgClose, fill = "Stock"), stat = "identity") +
    geom_line(aes(y = Popularity * scale_factor, color = "Popularity")) +
    theme_minimal()
}

# Main execution workflow
# Fetch stock data
stock_data <- fetch_stock_data("AAPL", "2020-01-01", "2023-12-31")

# Process stock data
processed_stock_data <- process_stock_data(stock_data, "AAPL.Close")

# Simulate search data
search_data <- simulate_search_popularity(
  years = c("2020", "2021", "2022", "2023"),
  quarters = c("Q1", "Q2", "Q3", "Q4")
)

# Merge stock and search data
merged_data <- merge_stock_and_search(processed_stock_data, search_data)

# Plot merged data
plot_data(merged_data)

# Validate merged data
testthat::test_that("fetch_stock_data returns valid data", {
  testthat::expect_true(nrow(processed_stock_data) > 0)
  testthat::expect_true(nrow(merged_data) > 0)
})
