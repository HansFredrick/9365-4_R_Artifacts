calculate_dividend <- function(principal, rate, frequency, start_date, end_date, additional_contributions) {
  
  compounding_periods <- switch(frequency,
                                "annually" = 1,
                                "semi-annually" = 2,
                                "quarterly" = 4,
                                "monthly" = 12,
                                stop("Invalid frequency"))
  

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
  
  all_contributions <- additional_contributions %>%
    mutate(contribution_date = as.Date(date)) %>%
    arrange(contribution_date) %>%
    filter(contribution_date >= start_date) 
  
  time_from_start_to_end <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
  interest_on_principal <- principal * ((1 + rate / compounding_periods)^(compounding_periods * time_from_start_to_end) - 1)
  interest_earned <- interest_earned + interest_on_principal
  

  for (i in seq_len(nrow(all_contributions))) {
    contribution <- all_contributions[i, ]
    contribution_date <- contribution$contribution_date
    contribution_amount <- contribution$amount
    
   
    time_in_years <- as.numeric(difftime(end_date, contribution_date, units = "days")) / 365.25
    
    
    interest_for_contribution <- contribution_amount * ((1 + rate / compounding_periods)^(compounding_periods * time_in_years) - 1)
   
    balance <- balance + contribution_amount
    interest_earned <- interest_earned + interest_for_contribution
  }
  
 
  final_balance <- balance + interest_earned
  return(list(final_balance = final_balance, total_interest = interest_earned))
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


