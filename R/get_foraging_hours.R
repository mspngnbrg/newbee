#' get_foraging_hours
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
get_foraging_hours <- function(df){

  # foraging suitability
  min_temp <- 12.5 # Vicens & Bosch (1996); 12 °C also found by Kovac & Schmaranzer 1996
  min_irridiance <- 229 # W/m² Vicens & Bosch (1996) Physiological and chemical ecology

  temp <- df$temperature_celsius
  irridiance <- df$global_irridiance

  foraging_hours <- numeric(length(temp))

  for (INDEX in 1:length(temp)){
    ifelse(temp[INDEX] >= min_temp && irridiance[INDEX] >= min_irridiance, foraging_hours[INDEX] <- 1, foraging_hours[INDEX] <- 0)
  }
  return(foraging_hours)
}
