# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

calculate_day_of_year <- function(month, day) {

  # month days
  x <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)

  day_of_year <- numeric(length(month))

  for (i in 1:length(month)){
    day_of_year[i] <- sum(x[1:month[i]]) +  day[i]
  }
  return(day_of_year)
}


