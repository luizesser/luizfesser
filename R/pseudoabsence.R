#' Pseudoabsence selection
#'
#' @description Obtain pseudoabsences for a species
#' @usage pseudoabsence(sp, sp_dat, env)
#' @param sp Species name
#' @param sp_dat SpatialPointsDataFrame of species presence records.
#' @param env Stack of environmental layers.
#' @details The function creates an envelope around presence records, then randomly sample n pseudoabsences out of the envelope.
#' n is equal the number of presence records. It will save the envelope as a .tif file in working directory.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @import sdm
#' @import raster
#' @import sp
#' @importFrom stats reformulate

pseudoabsence <- function(sp, sp_dat, env){
  if(!file.exists(paste0('bioclim_',sp,'.tif'))){
    print(paste0("Building SRE..."))
    d <- sdmData(reformulate(termlabels = names(env), response = names(sp_dat)),
                      train = sp_dat,
                      predictors = env)
    m <- sdm(~.,
                  data = d,
                  methods='bioclim')
    p <- predict(m, env,
                      filename=paste0('bioclim_',sp,'.tif'))
  } else {
    p <- raster(paste0('bioclim_',sp,'.tif'))
  }
  p[p[]>0] <- NA
  print(paste0("Masking Environment..."))
  env2 <- mask(env, p)
  print(paste0("Obtaining pseudoabsences..."))
  pa <- sampleRandom(env2, nrow(sp_dat), sp = TRUE, na.rm=T)
  pa2 <- as.data.frame(pa)
  pa2 <- pa2[,c("x","y",names(env))]
  colnames(pa2) <- c(colnames(sp_dat@coords),names(env))
  return(pa2)
}

