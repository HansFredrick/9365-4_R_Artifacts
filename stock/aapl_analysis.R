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
