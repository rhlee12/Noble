% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/last_date_time.R
\name{last.day.time}
\alias{last.day.time}
\title{Returns the end date and time within a specified month}
\usage{
last.day.time(end.month, time.agr)
}
\arguments{
\item{end.month}{Parameter of class character. The month in which the latest date and time should be returned.}

\item{time.agr}{What the temporal agregation of the returned time shoule be, in minutes.}
}
\value{
Latest date and time in the input month, offset from midnight by the input temporal agregation.
}
\description{
For a temporal agregation, the function will return the latest date and time of that aggregation. For example, when December 2017 is specified with a 30 minute agregation, the function will return "2017-12-31 23:30:00".
}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
