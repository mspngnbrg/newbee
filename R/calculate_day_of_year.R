#' calculate_day_of_year
#'
#' @param month
#' @param day
#'
#' @return
#' @export
#'
#' @examples
calculate_day_of_year <- function(month, day) {

  # month days
  x <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)

  day_of_year <- numeric(length(month))

  for (i in 1:length(month)){
    day_of_year[i] <- sum(x[1:month[i]]) +  day[i]
  }
  return(day_of_year)
}


