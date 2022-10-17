#' Tune algorithms in sdm package
#'
#' @description This function allows algorithms tunning in sdm package.
#' @usage tune_sdm(d, algo, tuneGrid, stat)
#' @param d The sdmData object.
#' @param algo Algorithm to be tested (see ?sdm::getmethodNames for possible entries).
#' @param tuneGrid A data.frame with the combination of parameters to be tunned. See ?expand.grid.
#' @param stat Evaluation statistics to be considered when tuning (see ?sdm::getEvaluation for all stats available).
#' @return A data.frame equal to tuneGrid, but with an extra column providing stat value.
#' @author Luíz Fernando Esser (luizesser@gmail.com)
#' https://luizfesser.wordpress.com
#' @examples
#' \dontrun{# Open data
#' df <- read.csv(system.file("external/pa_df.csv", package="sdm"))
#'
#' # Create sdmData
#' d <- sdmData(sp~b15+NDVI,train=df)
#' d
#'
#' # Create tune grid:
#' tuneGrid_gbm <- expand.grid(distribution = c('gaussian', 'laplace', 'bernoulli'),
#' n.trees = c(10, 100, 1000),
#' stringsAsFactors = F)
#'
#' # Run function:
#' tuning_result <- tune_sdm(d, algo='gbm', tuneGrid = tuneGrid_gbm, 'AUC')}
#' @import sdm
#' @importFrom plyr alply

tune_sdm <- function(d, algo, tuneGrid, stat){
  tuneList <- alply(tuneGrid, 1, as.list)
  result <- data.frame(tuneGrid, AUC=NA)
  for(i in 1:length(tuneList)){
    l <- list(tuneList[[i]])
    names(l) <- algo
    s <- sdm(data=d,methods=paste0(algo), seed=1, modelSettings=l)
    result$AUC[i] <- getEvaluation(s, stat = paste0(stat))[2]
  }
  return(result)
}


