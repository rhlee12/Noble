############################################################################################
#' @title  Plot 2D Wind Direction Histograms

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Produces a PNG of wind direction histograms for all valid levels TIS towers. Bins are
#' color-coded by final QF state, and the approximate bounds of the distorted flow fields are shown as
#' vertical lines.
#'
#'
#' @param site The 4-letter NEON site code.
#' @param bgn.month The fisrt month of data to use in plotting.
#' @param end.month The last month of data to use in plotting.
#' @param save.dir The save directory for the output plot.
#' @param dir.bins Optional. The number of bins in the histogram.
#'
#'
#' @return A PNG of level-by-level wind direction histograms saved to \code{save.dir}.
#'
#' @keywords process quality, data quality, consistency, commissioning, air temperature

#' @examples
#' \dontrun{
#' plot.wind.dir.hist(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
#' }

#' @export wind.dir.hist

# changelog and author contributions / copyrights
#   Robert Lee (2017-01-19)
#     original creation
#
##############################################################################################

wind.dir.hist=function(site, bgn.month, end.month, save.dir, dir.bins=72){

    windDirMean=NULL
    windDirFinalQF=NULL
    DFF=Noble::distorted.field(site)

    wind.data=Noble::pull.data(site = site, dp.id = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = tempdir())

    sensor.locs=seq(1, Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]-1, by=1)

    data.by.ml=lapply(sensor.locs, function(x) Noble::ml.extract(wind.data, ml=x))
    names(data.by.ml)=sensor.locs
    data.by.ml=lapply(sensor.locs, function(x)data.by.ml[[x]]=data.frame(data.by.ml[[x]], ML=rep(x, times=length(data.by.ml[[x]][,1]))))

    for(i in 1:length(data.by.ml)){
        colnames(data.by.ml[[i]])=gsub(pattern = "\\.\\d\\d\\d.\\d\\d\\d", replacement = "", x = colnames(data.by.ml[[i]]))
    }

    plot.data=lapply(data.by.ml, function(x) x[,colnames(x) %in% c("startDateTime", "windSpeedMean", "windDirMean", "windDirFinalQF", "ML")])

    plot=function(x){ggplot2::ggplot(data=x, ggplot2::aes(x=windDirMean, fill=factor(windDirFinalQF)))+
            ggplot2::geom_histogram(bins = dir.bins)+
            ggplot2::scale_fill_manual("windDirFinalQF",values = c("1"="#f79a7b", "0"="#abfcb1"))+
            ggplot2::scale_x_continuous(breaks = seq(from=0, to=360, by=40), limits = c(0,360))+
            ggplot2::geom_vline(xintercept = as.numeric(DFF$buff_low[1]))+
            ggplot2::geom_vline(xintercept = as.numeric(DFF$buff_hi[2]))+
            ggplot2::theme_minimal()+
            ggplot2::ggtitle(label = paste0("2D Wind Direction - ", site), subtitle = paste0("ML",unique(x$ML), ", ", bgn.month, " to ", end.month))

    }

    plots=lapply(plot.data, function(x) if(!all(is.na(x$windDirMean))){plot(x)}else{ggplot2::ggplot(data = data.frame())+
            ggplot2::geom_blank()+
            ggplot2::xlim(0, 360) +
            ggplot2::ylim(0, 100)+
            ggplot2::annotate(geom = "text", x=180, y = 50, label="NO DATA", size=5)})

    combo=gridExtra::arrangeGrob(ncol=1, nrow=length(plots), grobs = plots)

    #combo=do.call(what = gridExtra::marrangeGrob(ncol=1), args = lapply(plots, ggplot2::ggplotGrob))

    ggplot2::ggsave(filename = paste0(site, "_2DWind_hist_", bgn.month, "-", end.month, ".png"), plot = combo, device = "png", width = 8, height = 10.5, path = save.dir, units = "in", dpi = 300)
}
