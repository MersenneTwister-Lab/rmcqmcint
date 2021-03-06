% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcqmcint.R
\name{digitalnet.dimF2MinMax}
\alias{digitalnet.dimF2MinMax}
\title{get minimum and maximum F2 dimension number of DigitalNet}
\usage{
digitalnet.dimF2MinMax(digitalNetID, dimR)
}
\arguments{
\item{digitalNetID}{1:Niederreiter-Xing low WAFOM, 2:Sobol low wafom,
3:Sobol large dimension.}

\item{dimR}{dimention.}
}
\value{
supportd minimum and maximum F2 dimension number for specified digitalNet.
}
\description{
DigitalNetID:
\itemize{
\item{1:}{Niederreiter-Xing low WAFOM}
\item{2:}{Sobol low WAFOM}
\item{3:}{Sobol Point Set up to dimension 21201}
}
}
