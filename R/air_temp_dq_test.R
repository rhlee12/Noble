############################################################################################
#' @title  Test NEON Air Temperature data for Stability of Variance, Internal Consistency,
#' and External Consistency.

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates and site, the function will perform variance stability testing
#'  on overnight data (midnight to 4:00 local time), internal consistency checks on adjacent
#'  measurement locations, and external consistency checks with the closest NRCS or USCRN site.
#'
#'
#' @param site Parameter of class character. The NEON site data should be downloaded for.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param save.dir Parameter of class character. The local directory where data files should be saved.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning
#' @examples
#' \dontrun{
#' #Make a temporary direcotry for the example:
#' tempDir= tempdir()
#' pull.data(site = "CPER",
#' dp.id = "DP1.00002.001",
#' bgn.month = "2017-04",
#' end.month = "2017-05",
#' time.agr = 30,
#' package="basic",
#' save.dir= tempDir)
#' }

#' @export
#' @importFrom magrittr %>%

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#   Robert Lee (2018-04-24)
#     Function refinement
##############################################################################################

# bgn.month="2017-04"
# end.month="2017-05"
# site="CPER"
# save.dir="/Volumes/neon/Science/Science Commissioning Archive/SiteAndPayload/TisAirTempDataQuality/"

air.temp.dq.test=function(site, bgn.month, end.month, save.dir){
    neon.temp=NULL
    options(stringsAsFactors = FALSE)

    T1=NULL
    T2=NULL
    startDateTime=NULL
    value=NULL
    variable=NULL
    AIR_TEMPERATURE=NULL
    Date=NULL

    ## PART 1: Variance Stability
    saat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = save.dir)
    taat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = save.dir)

    test.data=base::cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])
    test.data$startDateTime=base::as.POSIXct(test.data$startDateTime)
    site.tz=as.character(Noble::tis_site_config$Time.Zone[(Noble::tis_site_config$SiteID==site)])

    attributes(test.data$startDateTime)$tzone=site.tz

    # Make a sequence of dates and times for the requested period
    bgn_temp = base::as.Date(base::paste0(bgn.month, "-01"), tz="UTC")
    end_temp = base::as.Date(base::paste0(end.month, "-01"), tz="UTC")
    bgn_temp = base::as.POSIXct(base::paste0(bgn.month, "-01"), tz="UTC")
    end_temp = base::as.POSIXlt(base::paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon=end_temp$mon+1
    #end_temp=end_temp-lubridate::minutes(30)-lubridate::seconds(1)

    frst.week=c(as.Date(paste0(bgn.month, "-01")), as.Date(paste0(bgn.month, "-01"))+7)
    last.week=c(as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1))-7, as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)))


    group.one=Noble::date.extract(data = test.data, bgn.date = bgn_temp, end.date =bgn_temp+lubridate::days(15))
    group.one=base::data.frame(startDateTime=group.one$startDateTime, group.one[,grepl(pattern = "tempSingleVariance", x = colnames(group.one))|grepl(pattern = "tempTripleVariance", x = colnames(group.one))])
    group.one=group.one[lubridate::hour(group.one$startDateTime) %in% c(0:5),]
    group.two=Noble::date.extract(data = test.data, bgn.date = end_temp-lubridate::days(15), end.date = end_temp-lubridate::minutes(30)-lubridate::seconds(1))
    group.two=base::data.frame(startDateTime=group.two$startDateTime, group.two[,grepl(pattern = "tempSingleVariance", x = colnames(group.two))|grepl(pattern = "tempTripleVariance", x = colnames(group.two))])
    group.two=group.two[lubridate::hour(group.two$startDateTime) %in% c(0:5),]

    f.test=c()
    for(i in 2:length(colnames(group.one))){
        f.test=append(f.test, try(stats::var.test(x=group.one[,i], y = group.two[,i], ratio = 1)$p.value, silent = T))
    }
    f.test[unlist(lapply(f.test, function(f) grepl(pattern = "not enough", x = f)))]=NA
    #if(class(f.test)=="try-error"){f.test=NA}
    f.test=as.numeric(f.test)
    mean=mean(stats::na.omit(f.test), na.rm = T)
    f.test=append(f.test, c("mean"=mean))
    variance=data.frame(ML=c(seq(Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$SiteID==site]), "Mean"), P_value=f.test)

## PLOTTING
    saat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = tempdir())
    taat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = tempdir())

    test.data=base::cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])

    test.time = c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                  "03:30:00", "04:00:00")

    night.data=test.data[which(strftime(test.data$startDateTime, format="%H:%M:%S", tz=site.tz) %in% test.time),]
    night.data=data.frame(startDateTime=night.data$startDateTime, night.data[,grepl(pattern = "Variance", x = colnames(night.data))])
    m.night=reshape2::melt(night.data, id.vars="startDateTime")
    m.night$startDateTime=as.POSIXct(m.night$startDateTime, tz = site.tz)
    m.night$testPeriod=0
    m.night$testPeriod[frst.week[2]>m.night$startDateTime&m.night$startDateTime>=frst.week[1]]=1
    m.night$testPeriod[last.week[2]>m.night$startDateTime&m.night$startDateTime>=last.week[1]]=1

    var.plot=ggplot2::ggplot(data=m.night, ggplot2::aes(x=startDateTime, y=value, color=as.factor(variable)))+
        ggplot2::geom_rect(
            ggplot2::aes(xmin=as.POSIXct(frst.week[1], tz=site.tz),
                         xmax = as.POSIXct(frst.week[2], tz=site.tz),
                         ymin = -Inf,
                         ymax = Inf),
            fill = 'gray',
            alpha = 0.01)+
        ggplot2::geom_rect(
            ggplot2::aes(xmin=as.POSIXct(last.week[2], tz=site.tz),
                         xmax = as.POSIXct(last.week[1], tz=site.tz),
                         ymin = -Inf,
                         ymax = Inf),
            fill = 'gray',
            alpha = 0.01)+
        ggplot2::geom_point()+
        ggplot2::theme_light()+
        ggplot2::ggtitle(paste0(site, " overnight variance values for radiation data products"), subtitle = "Gray boxes indicate test periods")+
        ggplot2::xlab("Date")+
        ggplot2::geom_smooth()

    ggplot2::ggsave(plot = var.plot, device = "png", width = 8, height = 5, filename = paste0(site, "_night_vars.png"), path = .data.route(site = site, save.dir = save.dir), units = "in")


    ## PART 2: Internal Consistancy
    internal.data=test.data[,grepl(pattern = "tempSingleMean", x = colnames(test.data))|grepl(pattern = "tempTripleMean", x = colnames(test.data))]
    #internal.data=internal.data[,!(grepl(pattern = "000.010", x=colnames(internal.data)))] #Remove ML1

    spearman=lapply(c( 1:(length(internal.data)-1)), function(l) try(stats::cor.test(x = internal.data[,l], y = internal.data[,l+1], method = "spearman", na.rm=T)))
    mls=unlist(lapply(c( 1:(length(internal.data)-1)), function(x) paste0("ML ", x, "-", x+1)))
    #spearman[lapply(spearman, function(f) f)]

    spearman=as.data.frame(do.call(rbind, spearman))
    sman.p=data.frame(Pair=mls, internal.p=unlist(spearman$p.value))
    sman.p$internal.p[grepl(x=sman.p$internal.p, pattern = "Error")]=NA

    ## PART 3: External Consistancy

    ext.sites=metScanR::getNearby(siteID = paste0("NEON:", site), radius = 20)

    ref.sites=character(0)
    if(length(ext.sites)>0){
        ref.sites=metScanR::getNetwork(ext.sites , network = c("NRCS", "USCRN"))
        temp=lapply(ref.sites, "[[", "location")
        ref.sites=ref.sites[lapply(temp, "[[", "date.end")=="present"]
        temp=lapply(ref.sites, "[[", "identifiers")
        ref.sites=ref.sites[any(unlist(lapply(temp, "[[", "idType")) %in% c("SCAN", "WBAN"))]
        # wban=as.data.frame(lapply(ref.sites, "[[", "identifiers"))[as.data.frame(lapply(ref.sites, "[[", "identifiers"))[,1]=="WBAN",2]

    }
    p_val=NA
    if(length(ref.sites)>0){
        ext.loc.info=do.call(rbind, lapply(ref.sites, "[[", "location"))


        closest.index=which.min(unlist(lapply(seq(length(ext.loc.info[,1])), function(i)
            geosphere::distm(c(ext.loc.info$longitude_dec[i], ext.loc.info$latitude_dec[i]),
                             c(Noble::tis_site_config$Longitude[Noble::tis_site_config$SiteID==site],
                               Noble::tis_site_config$Latitude[Noble::tis_site_config$SiteID==site])))))

        ref.site=ref.sites[[closest.index]]

        if(ref.site$platform=="USCRN"){
            ## NEED TO POINT TO METDOWNLOADR

           #-------->  ref.data=Noble::pull.USCRN.data(timeScale = "subhourly",
            #                                 stationID = as.numeric(ref.site$identifiers$id[ref.site$identifiers$idType=="WBAN"]),
            #                                 TimeBgn = bgn_temp,
            #                                 TimeEnd = end_temp,
            #                                 saveDir = save.dir)
            # ref.data$Date=as.POSIXct(ref.data$UTC_DATE, tz="UTC")
            #
            # if(nchar(as.character(ref.data$Date[1]))>10){
            #     frst30=grep(ref.data$Date[1:16], pattern = ":30:")
            #     ref.data=ref.data[-(1:(frst30-1)),]
            # }
            #
            #
            # ## Make 30 min direction data from refs
            # ref.data=data.frame(ref.data %>%
            #                         dplyr::group_by(Date = cut(Date, breaks="30 min")) %>%
            #                         dplyr::summarize(airTemp = mean(AIR_TEMPERATURE)))
            #
            # neon.temp=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)
            #
            # neon.data=data.frame(Date=as.POSIXct(neon.temp$startDateTime, tz = "UTC"), neon.temp=neon.temp$tempSingleMean.000.020)
            #
            #
            # ref.data$Date=as.POSIXct(ref.data$Date, tz="UTC")
            # #neon.data$Date=as.POSIXct(neon.data$Date, tz="UTC")
            #
            #
            # comp.data=merge(x=neon.data, y=ref.data)
            # correlation=stats::cor.test(x=comp.data$neon.temp, y=comp.data$airTemp, method = "spearman")
            # p_val=correlation$p.value

            p_val=NA
        }else if(ref.site$platform=="NRCS"){
            ref.data=RNRCS::grabNRCS.data(network = "SCAN",
                                          site_id = ref.site$identifiers$id[ref.site$identifiers$idType=="SCAN"],
                                          timescale = "hourly",
                                          DayBgn = bgn_temp,
                                          DayEnd = as.Date(end_temp))

            ref.data$Date=as.POSIXct(ref.data$Date, format="%Y-%m-%d %H:%M", tz=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site])

            simple.data=data.frame(
                Date=as.POSIXct(ref.data$Date, tz=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]),
                airTemp=(ref.data$Air.Temperature.Average..degF.-31)/1.8)


            simple.data$Date=as.POSIXct(format(simple.data$Date, tz="UTC", usetz = T), format="%Y-%m-%d %H:%M:%S")+
                lubridate::hours(as.POSIXct(bgn_temp, tz="UTC")-as.POSIXct(as.character(bgn_temp), tz=Noble::tis_site_config$Time.Zone[Noble::tis_site_config$SiteID==site]))
            #simple.data$Date=as.POSIXct(simple.data$Date)


            neon.data=data.frame(Date=as.POSIXct(saat.test.data$startDateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC"), neon.temp=saat.test.data$tempSingleMean.000.020)

            neon.data=data.frame(neon.data %>%
                                     dplyr::group_by(Date = cut(Date, breaks="60 min")) %>%
                                     dplyr::summarize(neon.temp = mean(neon.temp)))
            neon.data$Date=as.POSIXct(neon.data$Date, tz="UTC", use.tz=TRUE)-lubridate::hours(1) #align with SCAN timing

            #simple.data$Date=as.POSIXct(simple.data$Date)

            comp.data=merge(x=neon.data, y=simple.data, by="Date")
            correlation=stats::cor.test(x=comp.data$neon.temp, y=comp.data$airTemp, method = "spearman")
            p_val=correlation$p.value
        }
    }

    out=list("Variance Stability"=variance, "Internal Correlation"=sman.p, "External Correlation"=p_val)
    if(out$`Variance Stability`$P_value[length(out$`Variance Stability`$ML)]>0.01){var.result="Pass"}else{var.result="Fail"}
    if(all(out$`Internal Correlation`$internal.p<.001)){int.cor.result="Pass"}else{int.cor.result="Fail"}
    if(is.na(out$`External Correlation`)){ext.cor.result="No Test"}else if(out$`External Correlation`<0.001){ext.cor.result="Pass"}else{ext.cor.result="Fail"}
    if(all(c(var.result, int.cor.result, ext.cor.result)=="Pass")){result="Pass"}else{result="Fail"}

    data.dir=.data.route(site = site, save.dir = save.dir)
    utils::write.csv(x = out$`Variance Stability`, file = paste0(data.dir, "variance.csv"), row.names = F)
    utils::write.csv(x = out$`Internal Correlation`, file = paste0(data.dir, "internal_comparison.csv"), row.names = F)
    utils::write.csv(x = out$`External Correlation`, file = paste0(data.dir, "external_comparison.csv"), row.names = F)

    rslt.string=data.frame("Site"=site, "Begin.Month"=bgn.month, "end.month"=end.month, "Variance"=var.result, "Internal.Correlation"=int.cor.result, "External.Correlation"=ext.cor.result, "Overall"=result)

    if(file.exists(.result.route(save.dir = save.dir))){
        prev=utils::read.csv(.result.route(save.dir = save.dir))
        curr=rbind(prev, rslt.string)
        utils::write.csv(x = curr, file = .result.route(save.dir = save.dir), row.names = FALSE)
    }else{
       utils::write.csv(x=rslt.string, file = .result.route(save.dir = save.dir), row.names = FALSE)
    }



    return(result)
}
