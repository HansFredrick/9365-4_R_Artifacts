required_packages <- c("quantmod", "ggplot2", "dplyr", "lubridate", "testthat")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
lapply(required_packages, library, character.only = TRUE)

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
  stopifnot(!missing(symbol), !missing(start_date), !missing(end_date))
  stock_data <- tryCatch(
    getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE),
    error = function(e) stop("Error fetching stock data: ", e)
  )
  return(stock_data)
}
stock_data <- fetch_stock_data("AAPL", "2020-01-01", "2023-12-31")

#' Process Stock Data
#'
#' Aggregates stock data to calculate average closing prices by year and quarter.
#'
#' @param stock_data A data frame of stock data with a `Date` index.
#' @param close_column A string representing the name of the closing price column (e.g., "AAPL.Close").
#' @return A data frame with columns `Year`, `Quarter`, and `AvgClose`.
#'
#' @examples
#' process_stock_data(stock_data, "AAPL.Close")

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

simulate_search_popularity <- function(years, quarters, min_pop = 50, max_pop = 100) {
  stopifnot(length(years) > 0, length(quarters) > 0)
  expand.grid(Year = years, Quarter = quarters) %>%
    mutate(Popularity = round(runif(n(), min_pop, max_pop)))
}



search_data <- simulate_search_popularity(c("2020", "2021", "2022", "2023"), c("Q1", "Q2", "Q3", "Q4"))

#' Merge Stock and Search Data
#'
#' Merges processed stock data with simulated search popularity data.
#'
#' @param stock_data A data frame containing processed stock data (`Year`, `Quarter`, `AvgClose`).
#' @param search_data A data frame containing simulated search popularity data (`Year`, `Quarter`, `Popularity`).
#' @return A merged data frame with columns `Year`, `Quarter`, `AvgClose`, and `Popularity`.
#'
#' @examples
#' merge_stock_and_search(processed_stock_data, search_data)

merge_stock_and_search <- function(stock_data, search_data) {
  validate_columns(stock_data, c("Year", "Quarter"))
  validate_columns(search_data, c("Year", "Quarter"))
  merge(stock_data, search_data, by = c("Year", "Quarter"))
}

validate_stock_data <- function(stock_data) {
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    stop("Error: Stock data must not be NULL or empty.")
  }
}

validate_columns <- function(data, required_cols) {
  if (!all(required_cols %in% colnames(data))) {
    stop("Error: Missing required columns.")
  }
}

handle_error <- function(message, error = NULL) {
  stop(paste("Error:", message, error))
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

