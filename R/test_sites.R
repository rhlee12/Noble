############################################################################################
#' @title  Returns a list of sites where a given data product exists in a given time range

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a given data product ID and the start and end months, a list of sites where the data product is has data is returned.
#'
#' @param dp.id Parameter of class character. The data product ID of interest, must be in code format, eg. "DP1.00001.001"
#' @param bgn.month Parameter of class character. The start month of the sequence of interest.
#' @param end.month Parameter of class character. The (inclusive) end month of the sequence of interest.
#'
#' @return A list of sites where the data product is available between the two input months.
#'
#'
#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' wind=test.sites(dpId="DP1.00001.001")
#' }
#'
#' @seealso \code{\link{neon.avail}}, which returns a data frame of data product availability by site and month.
#'
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################

test.sites = function(dp.id, bgn.month, end.month){
    options(stringsAsFactors = F)
    prod.avail=neon.avail(dp.id = dp.id)
    bgn.avail=zoo::as.yearmon(bgn.month)
    end.avail=zoo::as.yearmon(end.month)
    tis_site_config=Noble::tis_site_config
    test.avail=rbind(prod.avail[prod.avail$Month==bgn.avail,], prod.avail[prod.avail$Month==end.avail,])

    indx.test.sites=c()
    for(i in 2:length(colnames(test.avail))){
        if(all(stats::complete.cases(test.avail[,i]))){
            temp=test.avail[,i]
            indx.test.sites=append(indx.test.sites, i)
        }
    }

    test.sites=colnames(test.avail[,indx.test.sites])
    test.sites=test.sites[test.sites %in% as.character(tis_site_config$site.id)]
    return(test.sites)
}
