% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_results.R
\name{parse.results}
\alias{parse.results}
\title{Parse a Raw Commissioning Results File for Most Recent Results}
\usage{
parse.results(test.dir, write.summary = T, target_bgn, target_end)
}
\arguments{
\item{test.dir}{The top level of a given test directory in the SCA (not the "Common" folder).}

\item{write.summary}{Logical, refering to if a simplified test summary should be written in
the Common folder of the specified directory. If TRUE, a summary_results.csv will be generated.}

\item{target_bgn}{Optional- if specified the parsed results will be restricted to the date range defined by target_bgn and target_end. Both must be entered, and in the format "YYYY-MM".}

\item{target_end}{Optional- if specified the parsed results will be restricted to the date range defined by target_bgn and target_end. Both must be entered, and in the format "YYYY-MM".}
}
\value{
The most recent test results from the 'results.csv' file in the
}
\description{
Given a directory in the SCA, this function output only the most recent test results
from the 'results.csv' file in the
}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
