% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soni_pq_test.R
\name{soni.pq.test}
\alias{soni.pq.test}
\title{Perform A Commissioning PQ Test on 3D Sonic Anemometer Data}
\usage{
soni.pq.test(site, bgn.month, end.month, save.dir, q.th = 95,
  v.th = 90, overwrite = FALSE)
}
\arguments{
\item{site}{Parameter of class character. The NEON site data should be downloaded for.}

\item{bgn.month}{Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.}

\item{end.month}{Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.}

\item{save.dir}{Parameter of class character. The local directory where data files should be saved.}

\item{q.th}{Parameter of class character. Optional. The threshold for data availability for a passing test, defaults to 0.95.}

\item{v.th}{Parameter of class character. Optional. The threshold for data validity for a passing test, defaults to 0.9.}
}
\value{
Writes data files to the specified directory.
}
\description{
For the specified dates, site, variables, and data product or name of family of data
products, data are downloaded and saved to the specifed directory. Process quality calculations are
 then performed and written to a results file in save.dir.
}
\examples{
\dontrun{
site = "CPER"
bgn.month = "2017-09"
end.month = "2017-10"
time.agr = 30
package="basic"
save.dir<-tempdir()
Noble::tis.pq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)
}

}
\author{
Robert Lee \email{rlee@battelleecology.org}\cr
}
\keyword{commissioning}
\keyword{data}
\keyword{gaps,}
\keyword{process}
\keyword{quality,}
