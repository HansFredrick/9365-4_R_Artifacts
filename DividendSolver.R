calculate_dividend <- function(principal, rate, frequency, start_date, end_date, additional_contributions) {
  # Define compounding periods per year
  compounding_periods <- switch(frequency,
                                "annually" = 1,
                                "semi-annually" = 2,
                                "quarterly" = 4,
                                "monthly" = 12,
                                stop("Invalid frequency"))
  
  # Initialize variables
  balance <- principal
  interest_earned <- 0
  
  # Combine initial amount and contributions
  #converts date column (strings) into contribution_date column (dateObject)
  all_contributions <- additional_contributions %>%
    mutate(contribution_date = as.Date(date)) %>%
    arrange(contribution_date) %>%
    filter(contribution_date >= start_date)  # Exclude contributions before the start date
  
  # Calculate interest on the initial principal
  time_from_start_to_end <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  interest_on_principal <- principal * ((1 + rate / compounding_periods)^(compounding_periods * time_from_start_to_end) - 1)
  interest_earned <- interest_earned + interest_on_principal
  
  # Iterate over contributions
  for (i in seq_len(nrow(all_contributions))) {
    contribution <- all_contributions[i, ]#extracting i th row
    contribution_date <- contribution$contribution_date# extracting value in contribution_date columun
    contribution_amount <- contribution$amount# extracting value in amount columun
    
    # Calculate time from contribution date to end date
    time_in_years <- as.numeric(difftime(end_date, contribution_date, units = "days")) / 365.25
    
    # Calculate interest for this contribution
    interest_for_contribution <- contribution_amount * ((1 + rate / compounding_periods)^(compounding_periods * time_in_years) - 1)
    
    # Update balance and interest earned
    balance <- balance + contribution_amount
    interest_earned <- interest_earned + interest_for_contribution
  }
  
  # Return the results
  final_balance <- balance + interest_earned
  return(list(final_balance = final_balance, total_interest = interest_earned))
}

# Example usage
initial_amount <- 5000  # Initial principal deposit
interest_rate <- 0.06  # 6% annual interest
frequency <- "annually"
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")

# Contributions
additional_contributions <- data.frame(
  date = c("2023-03-03", "2023-06-15"),  # Contribution dates
  amount = c(10000, 2000)  # Contribution amounts
)

result <- calculate_dividend(
  principal = initial_amount,
  rate = interest_rate,
  frequency = frequency,
  start_date = start_date,
  end_date = end_date,
  additional_contributions = additional_contributions
)

# Print results
cat("Final Balance:", round(result$final_balance, 2), "\n")
cat("Total Interest (Dividends):", round(result$total_interest, 2), "\n")
