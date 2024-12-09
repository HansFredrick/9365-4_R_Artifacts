library(tidyverse)
library(lubridate)

calculate_dividend <- function(principal, rate, frequency, start_date, end_date, additional_contributions) {
  # Standard compliant function definition
  compounding_periods <- get_compounding_periods(frequency)
  results <- process_calculations(principal, rate, compounding_periods, 
                                  start_date, end_date, additional_contributions)
  format_results(results)
}


get_compounding_periods <- function(frequency) {
  switch(tolower(frequency),
         "annually"      = 1,
         "semi-annually" = 2,
         "quarterly"     = 4,
         "monthly"      = 12,
         stop("Invalid frequency specified"))
}

initial_amount <- 5000
interest_rate <- 0.06  
frequency <- "annually"
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")




additional_contributions <- data.frame(
  date = c("2023-03-03", "2023-06-15"),
  amount = c(10000, 2000)  
)


result <- calculate_dividend(
  principal = initial_amount,
  rate = interest_rate,
  frequency = frequency,
  start_date = start_date,
  end_date = end_date,
  additional_contributions = additional_contributions
)
