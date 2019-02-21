############################################################################################
#' @title  Save Summary Graphs of Data Product Health by Month

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce summary plots of data
#' product availability and validity for their period of record at a site. By default, plots for all
#' sites with data product availability are generated and saved to the specified directory. If
#' \code{site} is specified, only plots for the site(s) passed to that parameter are generated and saved.
#'
#' @param dp.id Parameter of class character. The NEON data product code of the data product of interest.
#' @param save.dir The directory for data files and output PNGs to be saved to.
#' @param site Optional. Can specify single site or list of sites of interest.
#' @param pri.var Optional. Can specify the data product field of interest (e.g. "windDirMean" for 2D wind direction).
#'
#' Because the full period of record for all sites are queried,
#' this function can take a long time to execute.
#'
#' @return Outputs a a PDF of plots data on of all measurement levesl, with one PDF per site.
#' If only one site is specified, the GGPlot2 object for the summary plot is also returned,
#' for use in automated report writing.

#' @keywords process quality, data quality, gaps, commissioning, data product, health


#' @export
#'
# changelog and author contributions / copyrights
#   Robert Lee (2016-07-017)
#     original creation
#
#   Robert Lee (2017-11-21)
#     Working version
#
#   Robert Lee (2017-11-26)
#     Color Revamp
#
##############################################################################################
dp.survey=function(dp.id, save.dir, site, pri.var){

    if(missing(pri.var)){
        pri.var=Noble::tis_pri_vars$data.field[Noble::tis_pri_vars$dp.id==dp.id]
    }
    var.name=gsub(pattern = "mean", replacement = "", x = pri.var, ignore.case = T)

    dp.avail = Noble::neon.avail(dp.id = dp.id)
    dp.avail = cbind(Month=dp.avail[,1],  dp.avail[,which(colnames(dp.avail) %in% Noble::tis_site_config$SiteID)]) #This line pairs down to TIS only, remove once AIS bug is squashed

    if(missing(site)){
        dp.sites = colnames(dp.avail[,2:length(colnames(dp.avail))])
        #dp.sites = dp.sites[which(dp.sites %in% Noble::tis_site_config$SiteID)]
    }else{
        dp.sites=site
    }


    for(i in 1:length(dp.sites)){

        health.data=Noble::health.data(site = dp.sites[i], dp.id = dp.id)
        dp.avail[which(dp.avail$Month %in% zoo::as.yearmon(health.data$Month)),which(colnames(dp.avail)==dp.sites[i])]=health.data$Availability

    }
    dp.survey=dp.avail
    return(dp.survey)
}

