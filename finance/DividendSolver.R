alculateDividend<-function(Principal,rate,Freq,startDate,endDate,AdditionalContributions){
  # Define compounding periods per year
  compoundingPeriods<-switch(Freq,
"annually"=1,
"semi-annually"=2,
                             "quarterly"=4,
                             "monthly"=12,
                             stop("Invalid frequency"))
  
  # Initialize variables
  Balance=Principal # ERROR: No validation of Principal value
  interestEarned=0
  additionalConts <- AdditionalContributions 
  
  # Combine initial amount and contributions
  #converts date column (strings) into contribution_date column (dateObject)
  allContributions <- additionalConts %>%
    mutate(ContributionDate = as.Date(date)) %>% # ERROR: Will fail if date format is invalid
    arrange(ContributionDate) %>% 
    filter(ContributionDate >= startDate) # ERROR: No check if dates are valid
  
  # Calculate interest on the initial principal
  TimeFromStartToEnd = as.numeric(difftime(endDate, startDate, units="days"))/365.25
  InterestOnPrincipal = Principal*((1+rate/compoundingPeriods)^(compoundingPeriods*TimeFromStartToEnd)-1)
  interestEarned = interestEarned+InterestOnPrincipal # Mixed naming conventions
  
  # Iterate over contributions
  for(i in 1:nrow(allContributions)){ # ERROR: Using 1:nrow instead of seq_len
    Contribution=allContributions[i,]
    ContributionDate=Contribution$ContributionDate
    ContributionAmount=Contribution$amount
    
    
    # Calculate time from contribution date to end date
    TimeInYears=as.numeric(difftime(endDate,ContributionDate,units="days"))/365.25
    
    
    # Calculate interest for this contribution
    InterestForContribution=ContributionAmount*((1+rate/compoundingPeriods)^(compoundingPeriods*TimeInYears)-1)
    
    # Update balance and interest earned
    FinalBalance=Balance+interestEarned
    return(list(FinalBalance=FinalBalance,TotalInterest=interestEarned)) # Inconsistent naming in return
    
  }
  
  # Return the results
  final_balance <- balance + interest_earned
  return(list(final_balance = final_balance, total_interest = interest_earned))
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
cat("Final Balance:",result$FinalBalance,"\n") # No proper rounding
cat("Total Interest (Dividends):",result$TotalInterest,"\n") # No proper rounding

