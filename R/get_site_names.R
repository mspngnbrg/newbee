#' get_site_names
#'
#' @param siteNo
#'
#' @return
#' @export
#'
#' @examples
get_site_names <- function(df){
  siteNo <- df$siteNo
  site_name <- memisc::cases(
    siteNo == 1 -> "Foulum",
    siteNo == 2 -> "Hinnerup",
    siteNo == 3 -> "Flakkebjerg",
    siteNo ==4 -> "Krankerup",
    siteNo == 5 -> "Serra_da_Lousa",
    siteNo ==6 -> "Idanha_a_nova",
    siteNo == 340 -> "Flakkebjerg2020"
  )
  return(site_name)
}

