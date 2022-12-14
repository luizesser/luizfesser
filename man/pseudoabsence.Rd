% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pseudoabsence.R
\name{pseudoabsence}
\alias{pseudoabsence}
\title{Pseudoabsence selection}
\usage{
pseudoabsence(sp, sp_dat, env)
}
\arguments{
\item{sp}{Species name}

\item{sp_dat}{SpatialPointsDataFrame of species presence records.}

\item{env}{Stack of environmental layers.}
}
\description{
Obtain pseudoabsences for a species
}
\details{
The function creates an envelope around presence records, then randomly sample n pseudoabsences out of the envelope.
n is equal the number of presence records. It will save the envelope as a .tif file in working directory.
}
\author{
Luíz Fernando Esser (luizesser@gmail.com)
https://luizfesser.wordpress.com
}
