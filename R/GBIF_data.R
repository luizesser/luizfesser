#' Retrieve Species data from GBIF
#'
#' @description Calculate the area of a species weighting it by species occurrence or environmental suitability.
#' @usage GBIF_data(s)
#' @param s vector of species names.
#' @details Function to retrieve species data from GBIF.
#' @references
#' https://www.gbif.org
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' # Select species names:
#' s <- c("Araucaria angustifolia", "Paubrasilia echinata", "Eugenia uniflora")
#'
#' # Run function:
#' data <- GBIF_data(s)
#' @import rgbif
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @export

GBIF_data <- function(s){
  ids <- lapply(s, function(x) { name_suggest(q=x, rank = "species")$data$key[1]})
  ids <- unlist(ids)
  data <- lapply(ids, function(x) { y <- occ_data(taxonKey=x)
                                    if('decimalLatitude' %in% names(y$data)){
                                      y <- y$data[,c("species", "decimalLatitude","decimalLongitude")]
                                      return(y)
                                    }
                                  } )
  data <- bind_rows(data)
  data <- data.frame(data)
  data <- na.omit(data)
  return(data)
}


