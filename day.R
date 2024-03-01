get_day_name <- function(day_number) {
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  # Check if the input is a valid day number
  if (day_number >= 1 && day_number <= length(days)) {
    return(days[day_number])
  } else {
    return("Invalid day number. Please enter a number between 1 and 7.")
  }
}

get_day_name(2)
