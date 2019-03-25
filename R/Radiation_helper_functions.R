#Define two time periods for variance testing
.var.periods=function(bgn.month, end.month){
    if(bgn.month==end.month){
        test.days=lubridate::days_in_month(zoo::as.yearmon(bgn.month))
        half=as.numeric(unlist(strsplit(x=as.character(test.days/2), split = "\\."))[1])
    }else{
        test.days=abs(difftime(time1 = as.Date(paste0(bgn.month, "-01")), time2 = as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)), units = "days"))
        half=as.numeric(unlist(strsplit(x=as.character(test.days/2), split = "\\."))[1])
    }
    out=list(
        frst.per=c(as.Date(paste0(bgn.month, "-01")), as.Date(paste0(bgn.month, "-01"))+half),
        last.per=c(as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1))-half, as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)))
    )
    return(out)
}


.rad.var.test=function(single.var.data, bgn.month, end.month, raw.dir, site, plot=FALSE){

    periods=.var.periods(bgn.month = bgn.month, end.month = end.month)
    frst.per=periods$frst.per
    last.per=periods$last.per
    #Put into massive data frame
    # if(class(raw.var.data)=="list"){
    # var.data=do.call(cbind, raw.var.data)}else{var.data=raw.var.data}

    #var.data=data.frame(startDateTime=var.data[,1], var.data[,grepl(pattern = "variance", x = colnames(var.data), ignore.case = T)])
    #var.data=var.data[,-which(grepl(pattern = "*LW*", x = colnames(var.data)))]

    # Convert to local time
    time.zone=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]
    #browser()
    var.data=single.var.data

    var.data$startDateTime=as.POSIXct(var.data$startDateTime, tz="UTC")
    var.data$startDateTime=as.POSIXct(format(var.data$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)

    test.time = c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                  "03:30:00", "04:00:00")

    night.vars=var.data[which(strftime(var.data$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]

    y=night.vars[,2]
    x=seq_along(night.vars$startDateTime)

    if(all(!is.na(y))){
        out=stats::lm(y~x)
        slope=out$coefficients[2]
        plot(night.vars)
    }else{
        slope=NA
        }


    out=c(stream=colnames(night.vars)[2], slope=slope)

    return(out)
    #
    # #subset to first and last weeks
    # first.pop=var.data[frst.per[2]>var.data$startDateTime&var.data$startDateTime>=frst.per[1],]
    # last.pop=var.data[last.per[2]>var.data$startDateTime&var.data$startDateTime>=last.per[1],]
    #
    # #Subset to nightime conditions
    #
    #
    #
    #
    #
    # first.pop=first.pop[]
    # last.pop=last.pop[which(strftime(last.pop$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]
    #
    # ## Remove all NAs, do this after subsetting to nightime conditions
    # first.pop=first.pop[,-which(colSums(is.na(first.pop))==length(first.pop[,1]))]
    # last.pop=last.pop[,-which(colSums(is.na(last.pop))==length(last.pop[,1]))]
    #
    # f.test=stats::var.test(unlist(as.list(first.pop[,(2:length(colnames(first.pop)))])),# ------>f.test results####
    #                        unlist(as.list(last.pop[,(2:length(colnames(last.pop)))])))
    #
    # if(0.01>f.test$p.value){f.test.result="Fail"}else{f.test.result="Pass"} ################################################################
    #
    # message(paste0("Variance Stability Test: ", f.test.result))
    # utils::write.csv(x = data.frame(value=unlist(f.test)),file = paste0(raw.dir, "variance_stats.csv"))
    #

    #If variance has failed, make a plot of all variances for the full testg period
    # if(f.test.result=="Fail"&plot){
    #
    #     night.data=var.data[which(strftime(var.data$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]
    #     m.night=reshape2::melt(night.data, id.vars="startDateTime")
    #     m.night$startDateTime=as.POSIXct(m.night$startDateTime, tz = time.zone)
    #     m.night$testPeriod=0
    #     m.night$testPeriod[frst.per[2]>m.night$startDateTime&m.night$startDateTime>=frst.per[1]]=1
    #     m.night$testPeriod[last.per[2]>m.night$startDateTime&m.night$startDateTime>=last.per[1]]=1
    #
    #     var.plot=ggplot2::ggplot(data=m.night, ggplot2::aes(x=startDateTime, y=value, color=as.factor(variable)))+
    #         ggplot2::geom_rect(
    #             ggplot2::aes(xmin=as.POSIXct(frst.per[1]),
    #                 xmax = as.POSIXct(frst.per[2]),
    #                 ymin = -Inf,
    #                 ymax = Inf),
    #             fill = 'gray',
    #             alpha = 0.01)+
    #         ggplot2::geom_rect(
    #             ggplot2::aes(xmin=as.POSIXct(last.per[2]),
    #                 xmax = as.POSIXct(last.per[1]),
    #                 ymin = -Inf,
    #                 ymax = Inf),
    #             fill = 'gray',
    #             alpha = 0.01)+
    #         ggplot2::geom_point()+
    #         ggplot2::theme_light()+
    #         ggplot2::ggtitle(paste0(site, " overnight variance values for radiation data products"), subtitle = "Gray boxes indicate test periods")+
    #         ggplot2::xlab("Date")+
    #         ggplot2::geom_smooth()
    #
    #     ggplot2::ggsave(plot = var.plot, device = "png", width = 8, height = 5, filename = paste0(site, "_night_vars.png"), path = raw.dir, units = "in")
    #
    #     #lm(m.night$startDateTime ~ m.night$value)
    # }

    #return(f.test.result)
}
