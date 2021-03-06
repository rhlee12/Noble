% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind_dq_test.R
\name{wind.dq.test}
\alias{wind.dq.test}
\title{Downloads and performs data quality checks on NEON wind data}
\usage{
wind.dq.test(site, save.dir, bgn.month, end.month)
}
\arguments{
\item{site}{Parameter of class character. The NEON site data should be downloaded for.}

\item{save.dir}{Parameter of class character. The local directory where data files should be saved.}

\item{bgn.month}{Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.}

\item{end.month}{Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.}
}
\value{
Writes data files to the specified directory.
}
\description{
For the specified dates, site, variables, and data product or name of family of data products,
data are downloaded and saved to the specifed directory. Process quality calculations are then performed and written to a results file in save.dir.
}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
