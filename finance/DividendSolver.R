library(tidyverse)
library(lubridate)

#' Comprehensive validation function for all inputs
validate_inputs <- function(principal, rate, frequency, start_date, end_date, additional_contributions) {
  validate_numeric_inputs(principal, rate)
  validate_frequency(frequency)
  validate_dates(start_date, end_date)
  validate_contributions(additional_contributions)
}

# Core validation functions
validate_numeric_inputs <- function(principal, rate) {
  if (!is.numeric(principal) || principal <= 0)
    stop("Principal must be a positive number")
  if (!is.numeric(rate) || rate < 0 || rate > 1)
    stop("Rate must be between 0 and 1")
}

validate_dates <- function(start_date, end_date) {
  tryCatch({
    start <- as.Date(start_date)
    end <- as.Date(end_date)
    
    if (is.na(start) || is.na(end))
      stop("Invalid date format")
    if (end <= start)
      stop("End date must be after start date")
    
    list(start = start, end = end)
  }, error = function(e) {
    stop("Date validation error: ", e$message)
  })
}

validate_contributions <- function(contributions) {
  if (!is.data.frame(contributions))
    stop("Contributions must be a data frame")
  
  required_cols <- c("date", "amount")
  if (!all(required_cols %in% names(contributions)))
    stop("Contributions must have 'date' and 'amount' columns")
  
  if (!all(sapply(contributions$amount, is.numeric)))
    stop("Contribution amounts must be numeric")
  
  if (any(contributions$amount <= 0))
    stop("Contribution amounts must be positive")
}

validate_frequency <- function(frequency) {
  valid_frequencies <- c("annually", "semi-annually", "quarterly", "monthly")
  if (!tolower(frequency) %in% valid_frequencies)
    stop("Invalid frequency. Must be one of: ", 
         paste(valid_frequencies, collapse = ", "))
}

# Interest calculation functions
calculate_interest <- function(amount, rate, time, periods) {
  tryCatch({
    if (amount <= 0 || rate < 0 || time <= 0 || periods <= 0)
      return(0)
    
    amount * ((1 + rate/periods)^(periods * time) - 1)
  }, error = function(e) {
    warning("Interest calculation error: ", e$message)
    0
  })
}

get_compounding_periods <- function(frequency) {
  switch(tolower(frequency),
         "annually"      = 1,
         "semi-annually" = 2,
         "quarterly"     = 4,
         "monthly"      = 12,
         stop("Invalid frequency specified"))
}

calculate_contribution_interest <- function(contributions, rate, periods, end_date) {
  if(nrow(contributions) == 0) return(0)
  contribution_dates <- as.Date(contributions$date)
  times <- as.numeric(difftime(end_date, contribution_dates, units = "days")) / 365.25
  sum(mapply(calculate_interest,
             contributions$amount,
             MoreArgs = list(rate = rate, periods = periods),
             time = times))
}

#' Main calculation function
#' @param principal Initial investment amount
#' @param rate Annual interest rate (decimal)
#' @param frequency Compounding frequency
#' @param start_date Start date of investment
#' @param end_date End date of investment
#' @param additional_contributions Data frame of contributions
#' @return List containing calculation results
calculate_dividend <- function(principal, rate, frequency, start_date, end_date, additional_contributions) {
  tryCatch({
    # Validate all inputs
    validate_inputs(principal, rate, frequency, start_date, end_date, additional_contributions)
    
    # Get compounding periods
    periods <- get_compounding_periods(frequency)
    
    # Calculate initial interest
    time_total <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
    initial_interest <- calculate_interest(principal, rate, time_total, periods)
    
    # Calculate contribution interest
    contribution_interest <- calculate_contribution_interest(
      additional_contributions, rate, periods, end_date)
    
    # Calculate final results
    total_contributions <- sum(additional_contributions$amount)
    final_balance <- principal + total_contributions + initial_interest + contribution_interest
    
    list(
      final_balance = round(final_balance, 2),
      total_interest = round(initial_interest + contribution_interest, 2),
      total_contributions = round(total_contributions, 2),
      calculation_date = Sys.Date()
    )
  }, error = function(e) {
    list(error = TRUE, message = e$message)
  })
}

# Interactive interface
get_interactive_inputs <- function() {
  cat("\n=== Dividend Calculator ===\n")
  
  repeat {
    principal <- tryCatch({
      value <- as.numeric(readline("Enter initial investment amount: "))
      if(is.na(value) || value <= 0) stop("Invalid amount")
      value
    }, error = function(e) {
      cat("Please enter a valid positive number\n")
      NULL
    })
    if(!is.null(principal)) break
  }
  
  repeat {
    rate <- tryCatch({
      value <- as.numeric(readline("Enter annual interest rate (e.g., 0.06 for 6%): "))
      if(is.na(value) || value < 0 || value > 1) stop("Invalid rate")
      value
    }, error = function(e) {
      cat("Please enter a valid rate between 0 and 1\n")
      NULL
    })
    if(!is.null(rate)) break
  }
  
  cat("\nCompounding frequencies available:")
  cat("\n1. annually\n2. semi-annually\n3. quarterly\n4. monthly")
  repeat {
    frequency_choice <- readline("\nEnter number (1-4): ")
    frequency <- switch(frequency_choice,
                        "1" = "annually",
                        "2" = "semi-annually",
                        "3" = "quarterly",
                        "4" = "monthly",
                        NULL)
    if(!is.null(frequency)) break
    cat("Please enter a valid number (1-4)\n")
  }
  
  repeat {
    start_date <- tryCatch({
      as.Date(readline("Enter start date (YYYY-MM-DD): "))
    }, error = function(e) {
      cat("Please enter a valid date in YYYY-MM-DD format\n")
      NULL
    })
    if(!is.null(start_date) && !is.na(start_date)) break
  }
  
  repeat {
    end_date <- tryCatch({
      date <- as.Date(readline("Enter end date (YYYY-MM-DD): "))
      if(is.na(date) || date <= start_date) stop("Invalid end date")
      date
    }, error = function(e) {
      cat("Please enter a valid date after start date\n")
      NULL
    })
    if(!is.null(end_date)) break
  }
  
  contributions <- data.frame(date = character(), amount = numeric())
  while(TRUE) {
    add_contribution <- readline("\nAdd a contribution? (y/n): ")
    if(tolower(add_contribution) != "y") break
    
    repeat {
      cont_date <- tryCatch({
        date <- as.Date(readline("Enter contribution date (YYYY-MM-DD): "))
        if(is.na(date) || date < start_date || date > end_date) 
          stop("Date must be between start and end dates")
        date
      }, error = function(e) {
        cat("Please enter a valid date between start and end dates\n")
        NULL
      })
      if(!is.null(cont_date)) break
    }
    
    repeat {
      cont_amount <- tryCatch({
        amount <- as.numeric(readline("Enter contribution amount: "))
        if(is.na(amount) || amount <= 0) stop("Invalid amount")
        amount
      }, error = function(e) {
        cat("Please enter a valid positive number\n")
        NULL
      })
      if(!is.null(cont_amount)) break
    }
    
    contributions <- rbind(contributions,
                           data.frame(date = cont_date, amount = cont_amount))
  }
  
  list(
    principal = principal,
    rate = rate,
    frequency = frequency,
    start_date = start_date,
    end_date = end_date,
    additional_contributions = contributions
  )
}

#' Interactive calculator function
calculate_dividend_interactive <- function() {
  inputs <- get_interactive_inputs()
  
  result <- calculate_dividend(
    principal = inputs$principal,
    rate = inputs$rate,
    frequency = inputs$frequency,
    start_date = inputs$start_date,
    end_date = inputs$end_date,
    additional_contributions = inputs$additional_contributions
  )
  
  if (!is.null(result$error)) {
    cat("\nError:", result$message, "\n")
  } else {
    cat("\n=== Results ===\n")
    cat("Final Balance: $", format(result$final_balance, big.mark = ","), "\n")
    cat("Total Interest: $", format(result$total_interest, big.mark = ","), "\n")
    cat("Total Contributions: $", format(result$total_contributions, big.mark = ","), "\n")
  }
}