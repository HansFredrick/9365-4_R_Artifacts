library(tidyverse)
library(lubridate)


#' Calculate dividend returns with additional contributions
#' @param principal Initial investment amount
#' @param rate Annual interest rate (decimal)
#' @param frequency Compounding frequency
#' @param start_date Start date of investment
#' @param end_date End date of investment
#' @param additional_contributions Data frame of contributions
#' @return List containing calculation results
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


process_calculations <- function(principal, rate, periods, start_date, end_date, contributions) {
  initial_interest <- calculate_initial_interest(principal, rate, periods, 
                                                 start_date, end_date)
  contribution_interest <- calculate_contribution_interest(contributions, rate, 
                                                           periods, end_date)
  
  list(
    principal = principal,
    initial_interest = initial_interest,
    contribution_interest = contribution_interest,
    total_contributions = sum(contributions$amount)
  )
}


#Calculation Funtion

format_results <- function(results) {
final_balance <- results$principal + 
  results$total_contributions + 
  results$initial_interest + 
  results$contribution_interest

list(
  final_balance = round(final_balance, 2),
  total_interest = round(results$initial_interest + 
                           results$contribution_interest, 2),
  total_contributions = round(results$total_contributions, 2),
  calculation_date = Sys.Date()
)
}
