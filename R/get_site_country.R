#' get_site_country
#'
#' @param siteNo
#'
#' @return
#' @export
#'
#' @examples
get_site_country <- function(siteNo){
  memisc::cases(
    siteNo == 1 -> "DK",
    siteNo == 2 -> "DK",
    siteNo == 3 -> "DK",
    siteNo ==4 -> "DK",
    siteNo == 5 -> "PT",
    siteNo ==6 -> "PT",
    siteNo == 340 -> "DK"
  )
}
