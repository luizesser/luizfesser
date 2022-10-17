#' Compare two binary rasters
#'
#' @description This function allows to compare binary rasters to better see patterns.
#' @usage compare_binaries(r1,r2)
#' @param r1 Binary raster from present/current distribution.
#' @param r2 Binary raster from future/projected distribution.
#' @details This function will compare two rasters, returning absence, expansion, retraction and permanence. Absence (value = 1) is a cell where the species is absent in both r1 and r2. Expansion (value = 2) is a cell where species is absent in r1 and present in r2. Retraction (value = 3) is a cell where species is present in r1, but absent in r2. Finally, permanence (value = 4) is a cell where species is present in both r1 and r2.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' \dontrun{r <- compare_binaries(r1,r2)
#' plot(r, col=c("light gray", "blue", "red", "green"))}
#' @import raster
#' @export
compare_binaries <- function(r1,r2){
  r1 <- raster(r1)
  r2 <- raster(r2)
  r <- r1+r2
  r3 <- r1+(-1*r2)
  absence    <- r==0
  expansion  <- r3==-1
  retraction <- r3==1
  permanence <- r==2
  result <- absence+
            2*expansion+
            3*retraction+
            4*permanence
  cls <- data.frame(ID=1:4, cover=c("absence", "expansion", "retraction","permanence"))
  levels(result) <- cls
  return(result)
}
