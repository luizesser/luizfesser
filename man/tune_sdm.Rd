% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune_sdm.R
\name{tune_sdm}
\alias{tune_sdm}
\title{Tune algorithms in sdm package}
\usage{
tune_sdm(d, algo, tuneGrid, stat)
}
\arguments{
\item{d}{The sdmData object.}

\item{algo}{Algorithm to be tested (see ?sdm::getmethodNames for possible entries).}

\item{tuneGrid}{A data.frame with the combination of parameters to be tunned. See ?expand.grid.}

\item{stat}{Evaluation statistics to be considered when tuning (see ?sdm::getEvaluation for all stats available).}
}
\value{
A data.frame equal to tuneGrid, but with an extra column providing stat value.
}
\description{
This function allows algorithms tunning in sdm package.
}
\examples{
\dontrun{# Open data
df <- read.csv(system.file("external/pa_df.csv", package="sdm"))

# Create sdmData
d <- sdmData(sp~b15+NDVI,train=df)
d

# Create tune grid:
tuneGrid_gbm <- expand.grid(distribution = c('gaussian', 'laplace', 'bernoulli'),
n.trees = c(10, 100, 1000),
stringsAsFactors = F)

# Run function:
tuning_result <- tune_sdm(d, algo='gbm', tuneGrid = tuneGrid_gbm, 'AUC')}
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
