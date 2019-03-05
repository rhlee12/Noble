############################################################################################
#' @title  Generate Pairwise Comparison Plots for SAAT Measurements by ML

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Produces  PNG of air temperature consistency plots, in pairwise measurement levels.
#' Plots are of temperature readings from ML-n versus readings from ML-n+1. The raw data used in the
#' plot are not saved.
#'
#' @param site The 4-letter NEON site code.
#' @param bgn.month The fisrt month of data to use in plotting.
#' @param end.month The last month of data to use in plotting.
#' @param save.dir The save directory for the output plot.
#'
#' @return A PNG of level-by-level comparison plots in the specified \code{save.dir}.
#'
#' @keywords process quality, data quality, consistency, commissioning, air temperature

#' @examples
#' \dontrun{
#' air.temp.cnst.plot(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
#'}
#'
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-11-15)
#     original creation
#
##############################################################################################

air.temp.cnst.plot=function(site, bgn.month, end.month, save.dir){
    T1=NULL
    T2=NULL

    num.mls=Noble::tis_site_config$num.of.mls[site==Noble::tis_site_config$site.id]

    saat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
    taat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)

    airTemp=cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])

    plots=lapply(
        seq(num.mls-1),
        function(i){

            first=Noble::ml.extract(airTemp, ml=i)
            second=Noble::ml.extract(airTemp, ml=i+1)
            data=data.frame(T1=first[,3], T2=second[,3])

            ggplot2::ggplot(data, ggplot2::aes(x=T1, y=T2))+
                ggplot2::geom_point(colour="#18e068")+
                ggplot2::geom_abline(slope = 1, intercept = 0, color='#6818e0', size=0.75)+
                ggplot2::theme_bw()+
                ggplot2::xlab(paste0("ML", i))+
                ggplot2::ylab(paste0("ML", i+1))+
                ggplot2::ggtitle(paste0("ML-", i, " vs ML-", i+1, " Air Temperature"), subtitle = paste0(site, ", ", zoo::as.yearmon(bgn.month), " to ", zoo::as.yearmon(end.month)))

        }

    )
    combo=do.call(gridExtra::grid.arrange, lapply(plots, ggplot2::ggplotGrob))
    grDevices::graphics.off()

    ggplot2::ggsave(filename = paste0(site, "_", bgn.month,"-", end.month,  "_ML_AT_comparisons.png"),
                    plot = combo,
                    device = "png",
                    path = save.dir,
                    width = 7.5,
                    height = 10,
                    units = "in")

}
