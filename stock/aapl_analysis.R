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

process_stock_data <- function(stock_data) {
  stock_df <- data.frame(Date = index(stock_data), coredata(stock_data))
  processed <- stock_df %>%
    mutate(Quarter = paste0("Q", ceiling(as.numeric(format(Date, "%m")) / 3)),
           Year = format(Date, "%Y")) %>%
      group_by(Year, Quarter) %>%
    summarise(AvgClose = mean(AAPL.Close, na.rm = TRUE))
  
  return(processed)
}


processed_stock_data <- process_stock_data(stock_data)

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



create_overengineered_graphs <- function(data) {
  # Plot 1: Stock Price vs Popularity
  plot1 <- ggplot(data, aes(x = Quarter, group = Year)) +
    geom_line(aes(y = AvgClose, color = "Stock Price")) +
    geom_line(aes(y = Popularity * 100, color = "Popularity (Scaled)")) +
    facet_wrap(~Year, scales = "free_x") +
    ggtitle("Stock Price vs Popularity by Quarter (Why?)") +
    theme_minimal()
  print(plot1)
 
  # Plot 2: Growth Comparison
  comparison_plot <- ggplot(data, aes(x = Quarter)) +
   geom_bar(aes(y = AvgStockGrowth, fill = "Stock Growth"), stat = "identity", position = "dodge") +
   geom_bar(aes(y = AvgPopularityGrowth, fill = "Popularity Growth"), stat = "identity", position = "dodge") +
   geom_line(aes(y = DifferenceMetric * 10, group = 1, color = "Difference Metric"), size = 1, linetype = "dashed") +
   scale_y_continuous(sec.axis = sec_axis(~./10, name = "Difference Metric (Scaled)")) +
   ggtitle("Quarterly Comparison Since 2022")


 
  # Plot 3: Irrelevant Scatterplot
  plot3 <- ggplot(data, aes(x = StockGrowth, y = PopularityGrowth)) +
    geom_point(aes(color = Year), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggtitle("Scatterplot of Growth Metrics (Why?)") +
    theme_light()
  print(plot3)
}


### Execution ###
# Step 1: Fetch stock data
stock_data <- fetch_apple_stock_data()


# Step 2: Simulate search popularity
search_data <- simulate_search_data()


# Step 3: Merge and compare datasets
merged_data <- merge_and_compare(stock_data, search_data)


# Step 4: Create graphs
create_overengineered_graphs(merged_data)


