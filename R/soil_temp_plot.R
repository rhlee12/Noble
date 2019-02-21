############################################################################################
#' @title  Plot Soil Temperature Profiles at a Given Time
#'
#' @author Robert Lee
#'
#' @description At a given site and date-time, a plot of the soil temperature profile for
#' all five soil plots is produced.
#'
#' @param site Parameter of class character. The TIS site of interest.
#' @param date Parameter of class character. The date and time of interest, \strong{to the nearest half hour in UTC}. Format should be \code{YYYY-MM-DD HH:MM}.
#'
#' @return A facet plot soil temperature data by depth.
#'
#' @keywords process quality, data quality, gaps, commissioning
#'
#' @examples
#' \dontrun{plot=Noble::soil.temp.plot(site="CPER", date="2018-02-28 12:30")}
##############################################################################################

soil.temp.plot=function(site, date){
    dp.id="DP1.00041.001"
    value=NULL
    depth=NULL

    month=format(zoo::as.yearmon(date), "%Y-%m")
    date=as.POSIXct(date, tz="UTC", format="%Y-%m-%d %H:%M")

    data=Noble::pull.data(site = site, dp.id = dp.id, bgn.month = month, end.month = month, time.agr = 30, package = "basic", save.dir = tempdir(), complete.times = F)

    locs=Noble::pull.dp.locs(site=site, dp.id = dp.id)

    date_ta=data[data$startDateTime==date,]

    plot.data=data.frame(locs=locs$HOR.VER, depth=locs$zOffset)
    plot.data$plot=gsub(x=lapply(stringr::str_split(string = plot.data$locs, pattern = "\\."), "[[", 1), pattern = "00", replacement = "Plot ")
    plot.data$variable= colnames(date_ta)[grep(pattern = "soilTempMean", colnames(date_ta))]
    plot.data$value= as.numeric(date_ta[,grep(pattern = "soilTempMean", colnames(date_ta))])

    plot=ggplot2::ggplot(data = plot.data, ggplot2::aes(x=value, y=depth))+
        ggplot2::geom_path(color="dark red")+
        ggplot2::geom_point(color="red")+
        ggplot2::facet_wrap(facets = ~plot, nrow = 1)+
        ggplot2::ggtitle(paste0(site, " soil temperatures, ", date, " UTC"))+
        ggplot2::xlab("Temperature (Degrees C)")+
        ggplot2::ylab("Depth (meters)")+
        ggplot2::theme_light()

    return(plot)
}
