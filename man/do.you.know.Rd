% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{do.you.know}
\alias{do.you.know}
\title{Do you know?}
\usage{
do.you.know(graph, you, people, how.well = 1)
}
\arguments{
\item{graph}{a igraph network object created with the /link{elite.network} function.}

\item{you}{a character vector of names present in graph}

\item{people}{a character vector of names preferably present in graph}

\item{how.well}{a number that says how weak the weakest considered tie is. The higher the weaker.}
}
\value{
a numeric vector with the /link{graph.strength} of the individuals named in "you". The graph strength is the sum of weighted edges within the group "people".
}
\description{
Find out how well two groups of people know each other
}
\examples{
library(soc.elite)
data(den)
data(pe13)
graph         <- elite.network(den)
you           <- pe13$Name
people        <- has.tags(den, tags = c("Political party"))
how.well      <- 2
do.you.know(graph, you, people, how.well)
}

