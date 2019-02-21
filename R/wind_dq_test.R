############################################################################################
#' @title  Downloads and performs data quality checks on NEON wind data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, variables, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory. Process quality calculations are then performed and written to a results file in save.dir.
#'
#' @param site Parameter of class character. The NEON site data should be downloaded for.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param save.dir Parameter of class character. The local directory where data files should be saved.

#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @export


# changelog and author contributions / copyrights
#   Robert Lee (2017-07-20)
#     original creation
#
##############################################################################################



# #Test Block
# site="CPER"
#
# bgn.month="2017-07"
# end.month="2017-08"
#
# test = "TIS 2D Wind Speed and Direction Data Quality "
# testSubDir = "Tis2DWindSpeedDataQuality"
# if(grepl("darwin", version$os))
# {
#     mountPoint<-"/Volumes/neon/" #Mac
# }else{
#     mountPoint<-"N:/" #Windows
# }
# dirCommBase = paste0(mountPoint, "Science/Science Commissioning Archive/SiteAndPayload/")
# testFullDir=paste0(dirCommBase, testSubDir, "/")
# save.dir=testFullDir
#
# #API not working
# data=read.csv(paste0(site.dir, list.files(site.dir)[1]))

# Wind DQ Test

wind.dq.test=function(site, save.dir, bgn.month, end.month){
    raw.dir=NULL
UTC_DATE=NULL
NEON.dir=NULL
windSpeedMean.000.050=NULL
startDateTime=NULL
spearman.results=NA

    valid.sites=c("BART", "JERC", "KONZ", "KONA", "WOOD", "CPER", "JORN", "BARR", "HEAL")

    if(!site %in% valid.sites)(stop())

    domn=Noble::is_site_config$Domain[Noble::is_site_config$SiteID==site]
    site.dir=.data.route(site=site, save.dir = save.dir)

    site.MLs=1:Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]
    wind.MLs=site.MLs[-length(site.MLs)]

    rslt.dir=.result.route(save.dir = save.dir)
    data=Noble::pull.data(site = site, dp.id = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 2, package = "basic", save.dir = site.dir)

    speed=data.frame(data[,grepl(x=colnames(data), "windSpeedMean*")])
    dir=data.frame(data[,grepl(x=colnames(data), "windDirMean*")])
    cir.dir=circular::circular(x=dir, type = "angles", units = "degrees", template = "geographics", modulo = "asis")

    #ML speed internal comparison
    ML.pairwise=lapply(wind.MLs[-1], function(x) c(x-1, x))
    comp=lapply(ML.pairwise, function(x) stats::cor(speed[,x[1]], speed[,x[2]], use = "complete.obs", method = "spearman"))
    comp.out=data.frame(cbind(as.character(ML.pairwise), comp))
    comp.out[,1]=gsub(pattern = ", ", replacement = "-",x = comp.out[,1])
    comp.out[,1]=gsub(pattern = "c\\(|\\)", replacement = "",x = comp.out[,1])
    colnames(comp.out)=c("ml.pair", "speed")

    #ML Dir internal comparison
    dir.comp=lapply(ML.pairwise, function(x) circular::cor.circular(x = cir.dir[,x[1]], y = cir.dir[,x[2]], test = T))
    comp.out$dir=lapply(dir.comp, "[[", "cor")

    #external comparison
    if(site=="BART"){
        time.zone=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]

        offset=difftime(time1 = as.POSIXct("2018-01-01", tz=time.zone), time2=as.POSIXct("2018-01-01", tz="UTC"))

        temp=RNRCS::grabNRCS.data(network = "SCAN",
                                  site_id = 2069,
                                  timescale = "hourly",
                                  DayBgn =paste0(bgn.month, "-01"),
                                  DayEnd =stringr::str_sub(string =  as.character(Noble::last.day.time(end.month = end.month, time.agr = 1)), start = 1, end = 10)
        )
        ext.data=temp
        ext.data$Date=as.POSIXct(ext.data$Date, tz=time.zone)

        int.data=try(Noble::pull.data(site = site,
                                      dp.id = "DP1.00001.001",
                                      bgn.month = bgn.month,
                                      end.month = end.month,
                                      time.agr = 30,
                                      package = "basic",
                                      save.dir = .data.route(site, save.dir = save.dir))
        )
        int.data$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")#-lubridate::hours(offset+2)

        int.speed<-data.frame(int.data %>%
                                dplyr::group_by(startDateTime = cut(startDateTime, breaks="60 min")) %>%
                                dplyr::summarize(windSpeedMean.000.050 = mean(windSpeedMean.000.050)))
        int.speed$startDateTime=as.POSIXct(int.speed$startDateTime, tz="UTC")

        speed.data=merge(x=int.speed, y=ext.data, by.y = "Date", by.x = "startDateTime")

        #qplot(x=all.data$Solar.Radiation.Average..watt.m2., y=all.data$gloRadMean.000.060)
        ext.consist=stats::cor.test(x=speed.data$Wind.Speed.Average..mph., y = speed.data$windSpeedMean.000.050, conf.level = 0.95, method = "spearman", exact = F)$estimate
    }
    # }else{
    #     uscrn.site=as.character(Noble::rad_dq_info$nearestUSCRN[Noble::rad_dq_info$Site==site])
    #     temp=Noble::pull.USCRN.data(timeScale = "subhourly",
    #                                 stationID = uscrn.site,
    #                                 TimeBgn = paste0(bgn.month, "-01"),
    #                                 TimeEnd =  as.character(Noble::last.day.time(end.month = end.month, time.agr = 1)),
    #                                 saveDir = .data.route(site = site, save.dir = save.dir)
    #     )
    #
    #     ext.data=temp
    #     ext.data$UTC_DATE=as.POSIXct(ext.data$UTC_DATE, tz="UTC")
    #     utils::write.csv(x = ext.data, file = paste0(raw.dir, "USCRN_", uscrn.site, ".csv"), row.names = F)
    #
    #
    #     ext.rad=data.frame(UTC_DATE=ext.data$UTC_DATE, USCRN.rad=ext.data$SOLAR_RADIATION)
    #     int.dir=try(Noble::pull.data(site = site,
    #                                   dp.id = "DP1.00001.001",
    #                                   bgn.month = bgn.month,
    #                                   end.month = end.month,
    #                                   time.agr = 2,
    #                                   package = "basic",
    #                                   save.dir = .data.route(site, save.dir = save.dir))
    #     )
    #     int.dir$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")
    #     int.dir=data.frame(UTC_DATE=as.POSIXct(int.data$endDateTime, format="%Y-%m-%dT%H:%M:%SZ"), NEON.dir=int.dir[,max(which(grepl(x = colnames(int.dir), pattern = "windDirMean")))])
    #     int.dir=data.frame(int.dir[-(1:4),] %>%
    #                            dplyr::group_by(UTC_DATE = cut(UTC_DATE, breaks="5 min")) %>%
    #                            dplyr::summarize(NEON.dir = mean(NEON.dir)))
    #     int.rad$UTC_DATE=as.POSIXct(int.rad$UTC_DATE, tz="UTC")
    #
    #     ext.rad$UTC_DATE=as.POSIXct(ext.rad$UTC_DATE, tz="UTC")
    #
    #     all.data=merge(x=int.rad, y=ext.rad, by= "UTC_DATE")
    #     all.data=all.data[all.data$USCRN.rad>-5,]
    #
    #
    #
    #     #bothRad<-data.frame(cbind(extRad, int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")]))
    #
    #     spearman.results=stats::cor.test(all.data$NEON.dir, all.data$USCRN.rad, method = "spearman", conf.level = 0.95, exact = F)
    #
    #     ext.consist=spearman.results$estimate
    #
    #     corr.plot=ggplot2::qplot(x=all.data$NEON.rad, y=all.data$USCRN.rad)+
    #         ggplot2::ggtitle(label = paste0(site, "-", uscrn.site, " Rho: ", round(ext.consist, digits = 2)))+
    #         ggplot2::geom_abline(slope = 1, color='red')+
    #         ggplot2::coord_fixed()
    #     ggplot2::ggsave(filename = paste0(raw.dir, "raw_corr_plot.pdf"), plot = corr.plot, device = "pdf", width = 7.5, height = 10, units = "in", dpi = 300)
    # }
        utils::write.csv(x = data.frame(unlist(spearman.results)), file = paste0(raw.dir, "external_comparison.csv"), row.names = T)

}
