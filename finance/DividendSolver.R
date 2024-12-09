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


#Input Validation
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

check_parameters <- function(params) {
  mapply(function(param, type, name) {
    if (!inherits(param, type))
      stop(sprintf("%s must be of type %s", name, type))
  }, params, types, names(params))
}





#Calculation Function
safe_calculate_dividend <- function(principal, rate, frequency, start_date, 
                                    end_date, additional_contributions) {
  tryCatch({
    validate_inputs(principal, rate, frequency, start_date, 
                    end_date, additional_contributions)
    calculate_dividend(principal, rate, frequency, start_date, 
                       end_date, additional_contributions)
  }, error = function(e) {
    list(
      error = TRUE,
      message = e$message,
      timestamp = Sys.time()
    )
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

safe_date_conversion <- function(date_str) {
  tryCatch({
    date <- as.Date(date_str)
    if (is.na(date))
      stop("Invalid date format")
    date
  }, error = function(e) {
    stop("Date conversion error: ", e$message)
  })
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







safe_calculate_interest <- function(amount, rate, time, periods) {
  tryCatch({
    if (amount <= 0 || rate < 0 || time <= 0 || periods <= 0)
      return(0)
    
    amount * ((1 + rate/periods)^(periods * time) - 1)
  }, error = function(e) {
    warning("Interest calculation error: ", e$message)
    0
  })
}
calculate_contribution_interest_vectorized <- function(contributions, rate, 
                                                       periods, end_date) {
  contribution_dates <- as.Date(contributions$date)
  times <- as.numeric(difftime(end_date, contribution_dates, 
                               units = "days")) / 365.25
  
  mapply(safe_calculate_interest,
         contributions$amount,
         MoreArgs = list(rate = rate, periods = periods),
         time = times)
}
#cache handlers

recover_invalid_input <- function(input, default, type) {
  tryCatch({
    converted <- as(input, type)
    if (is.na(converted))
      default
    else
      converted
  }, error = function(e) {
    warning("Converting to default value: ", e$message)
    default
  })
}

recover_invalid_input <- function(input, default, type) {
  tryCatch({
    converted <- as(input, type)
    if (is.na(converted))
      default
    else
      converted
  }, error = function(e) {
    warning("Converting to default value: ", e$message)
    default
  })
}

additional_contributions() {
  tryCatch({
    validate_inputs(principal, rate, frequency, start_date, 
                    end_date, additional_contributions)
    calculate_dividend(principal, rate, frequency, start_date, 
                       end_date, additional_contributions)
  }, error = function(e) {
    list(
      error = TRUE,
      message = e$message,
      timestamp = Sys.time()
    )
  })
}



date_handler <- function() {
  cache <- new.env(parent = emptyenv())
  
  list(
    process = function(date_str) {
      if (exists(date_str, envir = cache))
        return(get(date_str, envir = cache))
      
      result <- safe_date_conversion(date_str)
      assign(date_str, result, envir = cache)
      result
    },
    clear = function() rm(list = ls(cache), envir = cache)
  )
}

calculation_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  list(
    calculate = function(key, expr) {
      if (exists(key, envir = cache))
        return(get(key, envir = cache))
      
      result <- expr()
      assign(key, result, envir = cache)
      result
    },
    clear = function() rm(list = ls(cache), envir = cache)
  )
}


process_large_contributions <- function(contributions, chunk_size = 1000) {
  split_contributions <- split(contributions, 
                               ceiling(seq_len(nrow(contributions))/chunk_size))
  
  results <- lapply(split_contributions, function(chunk) {
    result <- calculate_contribution_interest_vectorized(chunk, rate, 
                                                         periods, end_date)
    gc() # Cleanup after each chunk
    result
  })
  
  do.call(rbind, results)
}


#data recovery

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
