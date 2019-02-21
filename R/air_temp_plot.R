#' @title Air Temperature Plot
#' @author Robert Lee \email{rlee@battelleecology.org}\cr
#'
#' @description Plots and saves all temperature measurements at a site for a given date range.
#' @param site The NEON TIS site to generate plots for.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to plot.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to plot.
#' @param save.dir Directory where plots should be written.
#'
#' @return A data frame with data collected between \code{bgn.time} and \code{end.time}.
#'
#' @keywords data, subset, date,
#'
#' @examples
#' \dontrun{
#' save.dir=getwd()
#' bgn.month<-"2016-01"
#' end.month<-"2016-04"
#' }

# changelog and author contributions / copyrights
#   Robert Lee (2017-12-14)
#     original creation
#
##############################################################################################

air.temp.plot=function(bgn.month, end.month, site, save.dir){

    Date=NULL
    value=NULL
    variable=NULL

    num.mls=Noble::tis_site_config$Num.of.MLs[site==Noble::tis_site_config$SiteID]

    saat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
    taat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)

    airTemp=cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])

    temps=data.frame(Date=airTemp$startDateTime, airTemp[,grepl(pattern = "tempSingleMean", x = colnames(airTemp))|grepl(pattern = "tempTripleMean", x = colnames(airTemp))])
    melt.temps=reshape2::melt(temps, id.vars="Date")
    melt.temps$Date=as.POSIXct(melt.temps$Date)
    plot=ggplot2::ggplot(data=melt.temps, ggplot2::aes(x = Date, y = value, color=factor(variable)))+
        ggplot2::geom_path(name="Measurement")+
        ggplot2::theme_bw()+
        ggplot2::ggtitle(site)+
        ggplot2::ylab("Temperature (Degrees C)")

    if(!missing(save.dir)){
        ggplot2::ggsave(filename = paste0(site, "_AAT_", bgn.month, "_", end.month, ".png"), plot = plot, device = "png", path = save.dir, units = "in", width = 8, height = 4)
    }

    return(plot)
}

# TREE_ML4=Noble::ml.extract(data=Noble::pull.data(site = "CPER", bgn.month = bgn.month, end.month = end.month, dp.id = "DP1.00002.001", save.dir = .data.route("CPER", dir)), ml = 3)
# TREE_ML5=Noble::pull.data(site = "CPER", bgn.month = bgn.month, end.month = end.month, dp.id = "DP1.00003.001", save.dir = .data.route("CPER", dir))
#
# plot.data=data.frame("Date"=TREE_ML4$startDateTime, ML4=TREE_ML4$tempSingleMean.000.030, ML5=TREE_ML5$tempTripleMean.000.040)
# melt.plot=reshape2::melt(data = plot.data, id.vars="Date")
# melt.plot$Date=as.POSIXct(melt.plot$Date)
# ggplot2::ggplot(data=melt.plot, aes(x = Date, y = value, color=factor(variable)))+geom_path()+theme_bw()
#
