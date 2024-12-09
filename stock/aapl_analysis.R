if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(quantmod)
library(ggplot2)
library(dplyr)


#' Fetch Stock Data
#'
#' Retrieves stock data from Yahoo Finance for a specified symbol and date range.
#'
#' @param symbol A string representing the stock ticker symbol (e.g., "AAPL").
#' @param start_date A string in "YYYY-MM-DD" format representing the start date.
#' @param end_date A string in "YYYY-MM-DD" format representing the end date.
#' @return A data frame of stock data with the associated dates as the index.
#'
#' @examples
#' fetch_stock_data("AAPL", "2020-01-01", "2023-12-31")
#' @throws Error if parameters are missing or invalid.

fetch_stock_data <- function(symbol, start_date, end_date) {
  if (missing(symbol) || missing(start_date) || missing(end_date)) {
    stop("Error: 'symbol', 'start_date', and 'end_date' are required parameters.")
  }
  if (!lubridate::is.Date(as.Date(start_date)) || !lubridate::is.Date(as.Date(end_date))) {
    stop("Error: 'start_date' and 'end_date' must be valid dates in 'YYYY-MM-DD' format.")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    stop("Error: 'start_date' cannot be later than 'end_date'.")
  }
  stock_data <- tryCatch(
    getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) stop("Error fetching stock data: ", e)
  )
  return(stock_data)
}
stock_data <- fetch_stock_data("AAPL", "2020-01-01", "2023-12-31")


process_stock_data <- function(stock_data, close_column) {
  
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    stop("Error: 'stock_data' must not be NULL or empty.")
  }
  if (!close_column %in% colnames(stock_data)) {
    stop(paste0("Error: Column '", close_column, "' does not exist in the stock data."))
  }
  
  stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))
  quarter_info <- calculate_quarters(stock_df$Date)
  stock_df <- cbind(stock_df, quarter_info)
  
  processed <- stock_df %>%
    group_by(Year, Quarter) %>%
    summarise(AvgClose = mean(stock_df[[close_column]], na.rm = TRUE), .groups = "drop")
  
  return(processed)
}

processed_stock_data <- process_stock_data(stock_data, "AAPL.Close")

#' Simulate Search Popularity
#'
#' Generates simulated search popularity data for specified years and quarters.
#'
#' @param years A vector of strings representing years (e.g., c("2020", "2021")).
#' @param quarters A vector of strings representing quarters (e.g., c("Q1", "Q2")).
#' @param min_pop A numeric value representing the minimum popularity score (default: 50).
#' @param max_pop A numeric value representing the maximum popularity score (default: 100).
#' @return A data frame with columns `Year`, `Quarter`, and `Popularity`.
#'
#' @examples
#' simulate_search_popularity(c("2020", "2021"), c("Q1", "Q2"))

simulate_search_popularity <- function(years, quarters) {
   if (is.null(years) || length(years) == 0) {
    stop("Error: 'years' must not be NULL or empty.")
  }
  if (is.null(quarters) || length(quarters) == 0) {
    stop("Error: 'quarters' must not be NULL or empty.")
  }
  if (!is.numeric(min_pop) || !is.numeric(max_pop) || min_pop < 0 || max_pop < min_pop) {
    stop("Error: 'min_pop' and 'max_pop' must be numeric, and 'max_pop' must be greater than or equal to 'min_pop'.")
  }
  
  simulate_search_popularity <- function(years, quarters, min_pop = 50, max_pop = 100) {
  search_data <- expand.grid(Year = years, Quarter = quarters)
  search_data <- search_data %>%
    mutate(Popularity = round(runif(n(), min_pop, max_pop)))
  return(search_data)
}


}
search_data <- simulate_search_popularity(c("2020", "2021", "2022", "2023"), c("Q1", "Q2", "Q3", "Q4"))

merge_stock_and_search <- function(stock_data, search_data) {
  required_cols <- c("Year", "Quarter")
  if (!all(required_cols %in% colnames(stock_data))) {
    stop("Error: 'stock_data' must contain 'Year' and 'Quarter' columns.")
  }
  if (!all(required_cols %in% colnames(search_data))) {
    stop("Error: 'search_data' must contain 'Year' and 'Quarter' columns.")
  }
  
  if (!all(c("Year", "Quarter") %in% colnames(stock_data))) {
    stop("Stock data must contain 'Year' and 'Quarter' columns.")
  }
  if (!all(c("Year", "Quarter") %in% colnames(search_data))) {
    stop("Search data must contain 'Year' and 'Quarter' columns.")
  }
  
  merged <- merge(stock_data, search_data, by = c("Year", "Quarter"))
  return(merged)
}


simulate_search_popularity <- function(years, quarters, min_pop = 50, max_pop = 100) {
  search_data <- expand.grid(Year = years, Quarter = quarters)
  search_data <- search_data %>%
    mutate(Popularity = round(runif(n(), min_pop, max_pop)))
  return(search_data)
}



create_overengineered_graphs <- function(data) {

  plot1 <- ggplot(data, aes(x = Quarter, group = Year)) +
    geom_line(aes(y = AvgClose, color = "Stock Price")) +
    geom_line(aes(y = Popularity * 900, color = "Popularity (Scaled)")) +
    facet_wrap(~Year, scales = "free_x") +
    ggtitle("Stock Price vs Popularity by Quarter (Why?)") +
    theme_minimal()
  print(plot1)
 

  plot3 <- ggplot(data, aes(x = StockGrowth, y = PopularityGrowth)) +
    geom_point(aes(color = Year), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggtitle("Scatterplot of Growth Metrics (Why?)") +
    theme_light()
  print(plot3)
}



  ccreate_comparison_graph <- function(data, scale_factor = 10) {
  ggplot(data, aes(x = Quarter)) +
    geom_bar(aes(y = AvgStockGrowth, fill = "Stock Growth"), stat = "identity") +
    geom_line(aes(y = DifferenceMetric * scale_factor, group = 1, color = "Difference Metric")) +
    ggtitle("Comparison Graph")
}

#' Calculate Quarters and Years
#'
#' This function calculates the quarter (Q1, Q2, Q3, Q4) and the year from a given date column.
#' 
#' @param date_column A vector of dates (Date or character format).
#' @return A data frame with two columns: `Quarter` and `Year`.
#' 
#' @examples
#' dates <- as.Date(c("2020-01-15", "2021-05-20"))
#' calculate_quarters(dates)

calculate_quarters <- function(date_column) {
  data.frame(
    Quarter = paste0("Q", ceiling(as.numeric(format(date_column, "%m")) / 3)),
    Year = format(date_column, "%Y")
  )
}

comparison_plot <- create_comparison_graph(merged_data

stock_data <- fetch_apple_stock_data()

testthat::test_that("fetch_stock_data returns non-empty data", {
  data <- fetch_stock_data("AAPL", "2020-01-01", "2023-12-31")
  testthat::expect_true(nrow(data) > 0)
})


search_data <- simulate_search_data()



merged_data <- merge_and_compare(stock_data, search_data)



create_overengineered_graphs(merged_data)

