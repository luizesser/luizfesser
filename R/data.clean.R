#' Presence data cleaning wrapper
#'
#' @description Data cleaning procedure using CoordinateCleaner and a raster.
#' @usage data.clean(x, r = NULL)
#' @param x data.frame of species records. The output of GBIF_data.
#' @param r Optional. A Raster used to delete records in the same cell.
#' @details Function to clean presence data avoiding autocorrelation.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' # Select species names:
#' s <- c("Araucaria angustifolia", "Paubrasilia echinata", "Eugenia uniflora")
#'
#' # Run function:
#' data <- GBIF_data(s)
#'
#' # Clean coordinates:
#' data2 <- data.clean(data)
#'
#' @import CoordinateCleaner
#' @import raster
#' @export

data.clean <- function(x, r=NULL){
  x <- subset( x, !is.na("decimalLongitude") | !is.na("decimalLatitude"))
  x <- cc_cap( x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_cen( x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_dupl(x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_equ( x, lon = "decimalLongitude", lat = "decimalLatitude")
  x <- cc_inst(x, lon = "decimalLongitude", lat = "decimalLatitude", species = "species")
  x <- cc_val( x, lon = "decimalLongitude", lat = "decimalLatitude")
  x <- cc_sea( x, lon = "decimalLongitude", lat = "decimalLatitude")
  if(!is.null(r)){
    print('Raster identified, procceding with rasterized filter.')
    r <- raster(r)
    values(r) <- 1:ncell(r)
    x2 <- x
    coordinates(x2) <- 2:3
    cell_id <- extract(r, x2)
    x <- cbind(x, cell_id)
    x <- x[!duplicated(x[,c(1,4)]),-4]
  }
  return(x)
}




raster.clean <- function(x, r){
  r <- raster(r)
  values(r) <- 1:ncell(r)
  x2 <- x
  coordinates(x2) <- 2:3
  cell_id <- extract(r, x2)
  x <- cbind(x, cell_id)
  x <- x[!duplicated(x[,c(1,4)]),-4]
  return(x)
}



