############################################################################################
#' @title  Returns a data frame of data product availability by month

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, a data frame of the availabilty of that product
#' for all NEON instrumented sites is returned. The output of data product availability is best
#' interpreted with the base \code{View()} function.
#'
#' @param dp.id Parameter of class character. The NEON data product code of the data product of interest.

#' @return Outputs a data frame of data product availability by month, where 'x' indicates availability.
#'

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' wind<-neon.avail(dp.id = "DP1.00001.001")
#' }
#' @export
#'


# changelog and author contributions / copyrights
#   Robert Lee (2016-11-08)
#     original creation
#
#   Robert Lee (2017-07-17)
#     Updating function for Noble integration
#
#   Robert Lee (2018-04-10)
#     Fixing tis_pri_vars bug
#
##############################################################################################

neon.avail<-function(dp.id = "DP1.00001.001") {
    # dpInfo <- base::data.frame(utils::read.csv(
    #     "https://raw.githubusercontent.com/rhlee12/Data-Products/master/kpiList.csv",
    #     header = TRUE))
    # The earliest start date. May want to swithc to earliest date found via API.
    tis_pri_vars=Noble::tis_pri_vars

    NEONstrt <- base::as.POSIXct("2014-01-01",tz="GMT",format="%Y-%m-%d")

    # Date of the function call, endcap on returned data frame
    currMon <- base::as.POSIXct(base::Sys.Date(),tz="GMT")

    # Make a sequence of months between start and now
    months<-base::seq(NEONstrt, currMon, by = "month")

    # Get those icky posix times to nice year-month dates
    refMonths <- zoo::as.yearmon(months)

    # Return the availaiblity of a data product, as far as the API is concerned
    availability=data.frame(do.call(rbind, jsonlite::read_json(paste0("http://data.neonscience.org/api/v0/products/", dp.id))$data$siteCodes))

    availDF <-data.frame(refMonths)
    dataPrd <- base::unlist(tis_pri_vars$dp.name[match(dp.id, tis_pri_vars$dp.id)])
    dfNames <- c("Month", unlist(availability$siteCode))

    #Wrap around the API availability by site, to make data frame
    for(i in 1:length(availability$siteCode)){
        site <- availability$siteCode[i]
        availMonths <- zoo::as.yearmon(unlist(availability$availableMonths[i]))
        gotData <- refMonths %in% availMonths
        #Uncomment below to plot only 'x's when data available

        char<-base::as.character(gotData)
        temp<-sub("TRUE", "x", char)
        availOut <- sub("FALSE", "", temp)
        availDF <- base::cbind(availDF, availOut)

        # Also COMMENT the line below to get only 'x's.
        #availDF <- cbind(availDF, gotData)
    }

    # Pretty up the output, and return it
    colnames(availDF)<-dfNames
    return(availDF)
}
