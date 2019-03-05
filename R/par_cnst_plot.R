############################################################################################
#' @title  Generate Pairwise Comparison Plots for PAR by ML

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Produces  PNG of PAR and QL consistency plots, by pairwise measurement levels and soil
#' plots.
#' Plots are of PAR readings from ML-n versus readings from ML-n+1. The raw data used in the
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
#' par.cnst.plot(site="CPER", bgn.month="2017-05", end.month="2017-05", save.dir=getwd())
#' }
#'
#' @export

#'

# changelog and author contributions / copyrights
#   Robert Lee (2017-11-15)
#     original creation
#
##############################################################################################

par.cnst.plot=function(site, bgn.month, end.month, save.dir){
    T1=NULL
    T2=NULL

    num.mls=Noble::tis_site_config$num.of.mls[site==Noble::tis_site_config$site.id]

    par.data=Noble::pull.data(site = site, dp.id = "DP1.00024.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
    ql.par.data=Noble::pull.data(site = site, dp.id = "DP1.00066.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)

    #airTemp=cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])

    plots=lapply(
        seq(num.mls-1),
        function(i){

            first=Noble::ml.extract(par.data, ml=i)
            second=Noble::ml.extract(par.data, ml=i+1)
            data=data.frame(T1=first[,3], T2=second[,3])

            ggplot2::ggplot(data, ggplot2::aes(x=T1, y=T2))+
                ggplot2::geom_point(colour="#18e068")+
                ggplot2::geom_abline(slope = 1, intercept = 0, color='#6818e0', size=0.75)+
                ggplot2::theme_bw()+
                ggplot2::xlab(paste0("ML", i))+
                ggplot2::ylab(paste0("ML", i+1))+
                ggplot2::ggtitle(paste0("ML-", i, " vs ML-", i+1, " PAR"), subtitle = paste0(site, ", ", zoo::as.yearmon(bgn.month), " to ", zoo::as.yearmon(end.month)))
            # graphics.off()
        }
    )
    plots=append(plots,
                 lapply(
                     c(1, 3),
                     function(i){

                         first=Noble::sp.extract(ql.par.data, sp=i)
                         second=Noble::sp.extract(ql.par.data, sp=i+2)
                         data=data.frame(T1=first[,3], T2=second[,3])

                         ggplot2::ggplot(data, ggplot2::aes(x=T1, y=T2))+
                             ggplot2::geom_point(colour="#18e068")+
                             ggplot2::geom_abline(slope = 1, intercept = 0, color='#6818e0', size=0.75)+
                             ggplot2::theme_bw()+
                             ggplot2::xlab(paste0("SP", i))+
                             ggplot2::ylab(paste0("SP", i+2))+
                             ggplot2::ggtitle(paste0("SP-", i, " vs SP-", i+2, "QL PAR"), subtitle = paste0(site, ", ", zoo::as.yearmon(bgn.month), " to ", zoo::as.yearmon(end.month)))
                         # graphics.off()
                     }
                     )
    )
    combo=do.call(gridExtra::grid.arrange, lapply(plots, ggplot2::ggplotGrob))
    grDevices::graphics.off()

    ggplot2::ggsave(filename = paste0(site, "_", bgn.month,"-", end.month,  "_ML_AT_comparisons.png"), plot = combo, device = "png", path = save.dir, width = 7.5, height = 10, units = "in")
}
