#' Weighted Area
#'
#' @description Calculate the area of a species weighting it by species occurrence or environmental suitability.
#' @usage w_area(x)
#' @param x Raster of species projection.
#' @details Function to retrieve area values according to Esser et al. 2019.
#' @references
#' Esser, L. F., Neves, D. M., & Jarenkow, J. A. (2019). Habitat‐specific impacts of climate change in the Mata Atlântica biodiversity hotspot. Diversity and Distributions, 25(12), 1846-1856.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' \dontrun{
#' p1 <- raster(...)
#' w_area(p1)}
#' @import raster

w_area <- function(x){
  cell_size <- area(x, na.rm=TRUE, weights=FALSE)
  x <- cell_size*x
  result <- cellStats(x, sum)
  print(paste0(result," km2"))
  return(as.numeric(result))
}
