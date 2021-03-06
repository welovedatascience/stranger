% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_info.R
\name{get_info}
\alias{get_info}
\alias{get_info.stranger}
\title{stranger object  information}
\usage{
get_info(x, ...)

\method{get_info}{stranger}(x, simplify = TRUE, ...)
}
\arguments{
\item{x}{One object from stranger package}

\item{\dots}{Additional parameters - not used currently}

\item{simplify}{boolean: simplify or keep output as list}
}
\value{
matrix - metrics attributes (corresponding weird info), one row per weird.
}
\description{
Retrieve some information on the content of an object built with stranger package.
}
\examples{
\dontrun{
library(dplyr)
info <-  iris \%>\% select(-Species) \%>\% crazyfy() \%>\% stranger() \%>\% get_info()
info
}
}
