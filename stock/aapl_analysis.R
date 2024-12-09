# Load libraries (some unnecessary ones included)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("tidyr")) install.packages("tidyr", dependencies = TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

fetch_stock_data <- function() {
  getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2023-12-31", auto.assign = TRUE)
  stock_data <- AAPL
  return(stock_data)
}
stock_data <- fetch_stock_data()

compute_quarterly_averages <- function(stock_data) {
  stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))
  stock_df <- stock_df %>%
    mutate(Quarter = paste0("Q", ceiling(as.numeric(format(Date, "%m")) / 3)),
           Year = format(Date, "%Y")) %>%
    group_by(Year, Quarter) %>%
    summarise(
      AvgClose = mean(AAPL.Close, na.rm = TRUE),
      High = max(AAPL.High, na.rm = TRUE),
      Low = min(AAPL.Low, na.rm = TRUE)
    )
  return(stock_df)
}

processed_stock_data <- compute_quarterly_averages(stock_data)

simulate_search_popularity <- function() {
  search_data <- expand.grid(
    Year = c("2020", "2021", "2022", "2023"),
    Quarter = c("Q1", "Q2", "Q3", "Q4")
  )
  search_data$Popularity <- round(runif(nrow(search_data), 50, 100))
  return(search_data)
}
search_data <- simulate_search_popularity()

merge_stock_and_search <- function(stock_data, search_data) {
  merged <- merge(stock_data, search_data, by = c("Year", "Quarter"))
  return(merged)
}
merged_data <- merge_stock_and_search(processed_stock_data, search_data)
