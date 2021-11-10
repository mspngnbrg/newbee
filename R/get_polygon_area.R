#' get_polygon_area
#'
#' @param df1
#' @param df2
#'
#' @return
#' @export
#'
#' @examples
get_polygon_area <- function(df1, df2){
  df_1_id <- df1$UniquePolyID
  df_1_area <- df1$Area
  df_2_id <- df2$polygonId
  area <- numeric(length(df_2_id))
  for(i in 1:length(df_2_id)){
    for(j in 1:length(df_1_id)){
      # if IDs match, assign polygon area to the new area column
      if(df_2_id[i] == df_1_id[j]) area[i] <- df_1_area[j]
    }
  }

  if(min(area) < 1) print("At least one polygon without correct size! ")
  return(area)
}
