if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(quantmod)
library(ggplot2)
library(dplyr)


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

#function for simulating search popularity
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
  merged <- merge(stock_data, search_data, by = c("Year", "Quarterr"))
  return(merged)
}
merged_data <- merge_stock_and_search(processed_stock_data, search_data)



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


 # Extracted comparison graph.
  ccreate_comparison_graph <- function(data, scale_factor = 10) {
  ggplot(data, aes(x = Quarter)) +
    geom_bar(aes(y = AvgStockGrowth, fill = "Stock Growth"), stat = "identity") +
    geom_line(aes(y = DifferenceMetric * scale_factor, group = 1, color = "Difference Metric")) +
    ggtitle("Comparison Graph")
}

comparison_plot <- create_comparison_graph(merged_data

### Execution ###
# Step 1: Fetch stock data
stock_data <- fetch_apple_stock_data()


# Step 2: Simulate search popularity
search_data <- simulate_search_data()


# Step 3: Merge and compare datasets
merged_data <- merge_and_compare(stock_data, search_data)


# Step 4: Create graphs
create_overengineered_graphs(merged_data)

