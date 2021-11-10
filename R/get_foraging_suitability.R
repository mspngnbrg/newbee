get_foraging_suitability <- function(df){

  # foraging suitability
  min_temp <- 12 # 12 °C Kovac & Schmaranzer 1996
  min_irridiance <- 100 # as soon as there is light, bees will fly

  temp <- df$temperature_celsius
  irridiance <- df$global_irridiance

  foraging_hours <- numeric(length(temp))

  for (INDEX in 1:length(temp)){
    ifelse(temp[INDEX] >= min_temp && irridiance[INDEX] >= min_irridiance, foraging_hours[INDEX] <- 1, foraging_hours[INDEX] <- 0)
  }
  return(foraging_hours)
}
