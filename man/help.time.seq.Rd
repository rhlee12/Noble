% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_time_seq.R
\name{help.time.seq}
\alias{help.time.seq}
\title{Generates a time sequence over the specified interval}
\usage{
help.time.seq(from, to, time.agr)
}
\arguments{
\item{from}{Parameter of class character. The start time of the sequence.}

\item{to}{Parameter of class character. The end time of the sequence.}

\item{time.agr}{What the time difference between sequence values should be, in minutes.}
}
\value{
A time sequence over the specified interval.
}
\description{
A time sequence, with spacing between values given by the time.agr parameter
(eg. a time.agr value of 30 means there should be a 30 minute jump between times in the output sequence).
}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
