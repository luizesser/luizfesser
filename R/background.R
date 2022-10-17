#' Background data selection
#'
#' @description Obtain background information for a species
#' @usage background(sp, sp_dat, env)
#' @param sp Species name
#' @param sp_dat SpatialPointsDataFrame of species presence records.
#' @param env Stack of environmental layers.
#' @details The function creates an envelope around presence records. All the data within the envelope is considered background.
#' It will save the envelope as a .tif file in working directory.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @import sdm
#' @import raster
#' @import sp
#' @importFrom stats reformulate

background <- function(sp, sp_dat, env){
  print(paste0("Building SRE..."))
  if(!file.exists(paste0('bioclim_',sp,'.tif'))){
    d <- sdmData(reformulate(termlabels = names(env), response = names(sp_dat)),
                 train = sp_dat,
                 predictors = env)
    m <- sdm(~.,
             data = d,
             methods='bioclim')
    p <- predict(m, env, filename=paste0('bioclim_',sp,'.tif'))
  } else {
    p <- raster(paste0('bioclim_',sp,'.tif'))
  }
  p[p[]>0] <- 1
  p[p[]<1] <- NA
  print(paste0("Obtaining environmental background..."))
  coord <- as.data.frame(coordinates(p))
  coordinates(coord) <- c(1,2)
  bg <- extract(p, coord)
  bg <- coord[!is.na(bg)]
  bg <- as.data.frame(cbind(bg@coords,extract(env, bg)))
  bg <- na.omit(bg)
  bg <- bg[,c("x","y",names(env))]
  colnames(bg) <- c(colnames(sp_dat@coords),names(env))
  return(bg)
}
