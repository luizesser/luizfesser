#' Retrieving IUCN’s RedList Status for Single or Multiple Species
#'
#' @description This function allows you to retrieve  IUCN’s RedList Status for
#' a single or multiple species.
#' @usage RedList_status(s,k)
#' @param s Species name in a binomial. e.g.: s <- 'Araucaria angustifolia'. s
#' can also be a vector with multiple species.
#' @param k Tolken provided by IUCN.
#' @details Note that to have access to IUCN’s database, you must first get a tolken (an access code). This can be done through a formular in this website: https://apiv3.iucnredlist.org/api/v3/token. It will take a few days to receive this tolken, so don’t be hurry when doing that.
#' @references https://apiv3.iucnredlist.org
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' s <- c('Araucaria angustifolia', 'Ilex paraguariensis')
#' result <- RedList_status(s,k)
#' @import taxize
#' @export


RedList_status <- function(s, k){
  info  <- iucn_summary(s, key=k)
  status <- iucn_status(info, key=k)
  status <- as.data.frame(status)
  return(status)
}
