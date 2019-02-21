############################################################################################
#' @title  Return TIS Data in a Specific Date-time Range

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, package parameters, and data product or name of family
#' of data products, data are downloaded and returned. Data for the month(s) spanned in the requested
#' dates will be saved to the \code{save.dir}, if specified.
#'
#' @param site Parameter of class character. The NEON site data should be downloaded for.
#' @param dp.id Parameter of class character. The data product code in question. See
#' \code{Noble::tis_pri_vars} for a selected list of data product names and codes, or
#' \url{http://data.neonscience.org/data-product-catalog} for a complete list.
#' @param bgn.date Parameter of class character. A date/time in the format yyyy-mm-dd HH:MM:SS,
#' representing the first timestamp to return data for.
#' @param end.date Parameter of class character. A date/time in the format yyyy-mm-dd HH:MM:SS,
#' representing the last timestamp to return data for.
#' @param time.agr Parameter of class numeric. The data agregation interval requested, must be
#' 1, 2, 5, or 30.
#' @param package Parameter of class character. Optional. The type of data package to be
#' returned If not specified, defaults to "basic".
#' @param save.dir Parameter of class character. Optional. The local directory where data files
#' should be saved. If unspecified, defaults to a temporariy directory given by \code{base::tempdir()}.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning
#'
#' @examples
#' \dontrun{
#' #Make a temporary direcotry for the example:
#' tempDir<- tempdir()
#' pull.date(site = "CPER", dp.id = "DP1.00002.001", bgn.month = "2017-03-15 00:00:00",
#' end.month = "2017-03-16 00:00:00", time.agr = 30, package="basic", save.dir= tempDir)
#' }




# changelog and author contributions / copyrights
#   Robert Lee (2018-01-31)
#     original creation
#
##############################################################################################

pull.date=function(site, dp.id, bgn.date, end.date, time.agr, package, save.dir){
    #if(missing(time.agr)){time.agr=30}

    if(base::missing(package)){package="basic"}
    if(base::missing(save.dir)){save.dir=tempdir()}

    bgn.month=base::substr(bgn.date, 1, 7)
    end.month=base::substr(end.date, 1, 7)

    out=Noble::date.extract(data = Noble::pull.data(site = site,
                                                dp.id = dp.id,
                                                bgn.month = bgn.month,
                                                end.month = end.month,
                                                time.agr = time.agr,
                                                package = package,
                                                save.dir = save.dir),
                        bgn.date = bgn.date,
                        end.date = end.date)
    base::return(out)
}
