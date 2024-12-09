alculateDividend<-function(Principal,rate,Freq,startDate,endDate,AdditionalContributions){
  # Define compounding periods per year
  compoundingPeriods<-switch(Freq,
"annually"=1,
"semi-annually"=2,
                             "quarterly"=4,
                             "monthly"=12,
                             stop("Invalid frequency"))
  
  # Initialize variables
  balance <- principal
  interest_earned <- 0
  breakdown <- data.frame(
    type = character(),
    date = as.Date(character()),
    amount = numeric(),
    interest = numeric(),
    dividend = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Combine initial amount and contributions
  # Converts date column (strings) into contribution_date column (Date object)
  all_contributions <- additional_contributions %>%
    mutate(contribution_date = as.Date(date)) %>%
    arrange(contribution_date) %>%
    filter(contribution_date >= start_date)  # Exclude contributions before the start date
  
  # Calculate interest on the initial principal
  time_from_start_to_end <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  interest_on_principal <- principal * ((1 + rate / compounding_periods)^(compounding_periods * time_from_start_to_end) - 1)
  
  # Update interest and add to breakdown
  interest_earned <- interest_earned + interest_on_principal
  breakdown <- rbind(breakdown, data.frame(
    type = "principal",
    date = start_date,
    amount = principal,
    interest = interest_on_principal,
    dividend = interest_on_principal,
    stringsAsFactors = FALSE
  ))
  
  # Iterate over contributions
  for (i in seq_len(nrow(all_contributions))) {
    contribution <- all_contributions[i, ]  # Extracting the i-th row
    contribution_date <- contribution$contribution_date  # Extracting value in contribution_date column
    contribution_amount <- contribution$amount  # Extracting value in amount column
    
    # Calculate time from contribution date to end date
    TimeInYears=as.numeric(difftime(endDate,ContributionDate,units="days"))/365.25
    
    
    # Calculate interest for this contribution
    InterestForContribution=ContributionAmount*((1+rate/compoundingPeriods)^(compoundingPeriods*TimeInYears)-1)
    
    # Update balance and interest earned
    balance <- balance + contribution_amount
    interest_earned <- interest_earned + interest_for_contribution
    
    # Add the contribution details to the breakdown
    breakdown <- rbind(breakdown, data.frame(
      type = "contribution",
      date = contribution_date,
      amount = contribution_amount,
      interest = interest_for_contribution,
      dividend = interest_for_contribution,
      stringsAsFactors = FALSE
    ))
  }
  
  # Return the results
  final_balance <- balance + interest_earned
  return(list(final_balance = final_balance, total_interest = interest_earned, breakdown = breakdown))
}

# Example usage
initial_amount = 5000
Interest_Rate = 0.06 # Inconsistent naming
frequency="annually"
Start_Date = as.Date("2023-01-01")
End_Date = as.Date("2023-12-31")


# Contributions
Additional_Contributions = data.frame(
  date=c("2023-03-03","2023-06-15"),
  amount=c(10000,2000)
) # No validation of contribution data

result=calculateDividend(
  Principal=initial_amount,
  rate=Interest_Rate,
  Freq=frequency,
  startDate=Start_Date,
  endDate=End_Date,
  AdditionalContributions=Additional_Contributions
) # No error handling for function call

# Print results
cat("Final Balance:", round(result$final_balance, 2), "\n")
cat("Total Interest (Dividends):", round(result$total_interest, 2), "\n")

# Print the breakdown of the calculation
print(result$breakdown)
