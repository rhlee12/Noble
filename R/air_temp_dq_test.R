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
    message(paste0("Testing ", site))

    site.dir=Noble:::.data.route(site=site, save.dir = save.dir)

    saat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = site.dir)
    taat.test.data=Noble::pull.data(site = site, dp.id = "DP1.00003.001", bgn.month = bgn.month, end.month = end.month, time.agr = 1, package = "basic", save.dir = site.dir)

    test.data=base::cbind(saat.test.data, taat.test.data[,(3:length(colnames(taat.test.data)))])

    #attributes(test.data$startDateTime)$tzone=site.tz
    ## PART 1: Variance Stability ####

    var.cols=grep(colnames(test.data), pattern = "variance", ignore.case = T)

    ## PLOTTING and TESTING
    pdf(file = paste0(site.dir, site, "_var_plots.pdf"), width = 10, height = 7.5, paper = "USr")
    out=lapply(var.cols,
               function(c) .var.drift.test(single.var.data=test.data[,c(1,c)],
                                           raw.dir=site.dir,
                                           site=site)
               )
    grDevices::dev.off()

    out %>%
        do.call(what = rbind) %>%
        data.frame() %>%
        `colnames<-`(value=c("stream", "slope")) -> stable.vars

    stable.vars=stable.vars[!is.na(stable.vars$slope),]
    stable.vars$slope=as.numeric(stable.vars$slope)
    stable.vars$overall=rep("Fail", times=nrow(stable.vars))
    # stable.vars$test0.1="Fail"
    # stable.vars$test0.01="Fail"
    # stable.vars$test0.001="Fail"
    #
    # stable.vars$test0.1[stable.vars$slope<0.1]="Pass"
    stable.vars$overall[stable.vars$slope<0.01]="Pass"
    # stable.vars$test0.001[stable.vars$slope<0.001]="Pass"

    utils::write.csv(x = stable.vars, file = paste0(site.dir, "variance_stats.csv"))

    drift.test="No Test"

    if(all(stable.vars$overall=="Pass")){drift.test="Pass"}else if(any(stable.vars$overall=="Fail")){drift.test="Fail"}

    message(paste0("Variance Stability (Drift test): ", drift.test))

    #.air.var.plotting
    message("Variance testing completed")

    ## PART 2: Internal Consistancy ####
    internal.data=test.data[,grepl(pattern = "tempSingleMean", x = colnames(test.data))|grepl(pattern = "tempTripleMean", x = colnames(test.data))]
    #internal.data=internal.data[,!(grepl(pattern = "000.010", x=colnames(internal.data)))] #Remove ML1

    spearman=lapply(c( 1:(length(internal.data)-1)), function(l) try(stats::cor.test(x = as.numeric(internal.data[,l]), y = as.numeric(internal.data[,l+1]), method = "spearman", na.rm=T)))
    mls=unlist(lapply(c( 1:(length(internal.data)-1)), function(x) paste0("ML ", x, "-", x+1)))
    #spearman[lapply(spearman, function(f) f)]

    spearman=as.data.frame(do.call(rbind, spearman))
    sman.p=data.frame(Pair=mls, internal.p=unlist(spearman$p.value))
    sman.p$internal.p[grepl(x=sman.p$internal.p, pattern = "Error")]=NA

    utils::write.csv(x = sman.p, file = paste0(site.dir, "internal_comparison.csv"), row.names = F)
    int.cor.result="No Test"
    if(all(sman.p$internal.p<.001, na.rm=T)){int.cor.result="Pass"}else{int.cor.result="Fail"}

    message(paste0("Internal correlation testing: ", int.cor.result))

    if(int.cor.result=="Fail"){
        Noble::air.temp.cnst.plot(site=site, bgn.month = bgn.month, end.month = end.month, save.dir = site.dir)
    }

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

    bgn_temp=paste0(bgn.month, "-01")
    end_temp=Noble::last.day.time(end.month = end.month, time.agr = 1)

    if(length(ref.sites)>0){
        ext.loc.info=do.call(rbind, lapply(ref.sites, "[[", "location"))


        closest.index=which.min(unlist(lapply(seq(length(ext.loc.info[,1])), function(i)
            geosphere::distm(c(ext.loc.info$longitude_dec[i], ext.loc.info$latitude_dec[i]),
                             c(Noble::tis_site_config$longitude[Noble::tis_site_config$site.id==site],
                               Noble::tis_site_config$latitude[Noble::tis_site_config$site.id==site])))))

        ref.site=ref.sites[[closest.index]]

        if(ref.site$platform=="USCRN"){
            ## NEED TO POINT TO METDOWNLOADR

            ref.data=metget::getUSCRNData(temp_agg = "subhourly", sid = ref.site$identifiers$id[ref.site$identifiers$idType=="WBAN"],
                                          start_date = bgn_temp, end_date = end_temp)

            #-------->  ref.data=Noble::pull.USCRN.data(timeScale = "subhourly",
            #                                 stationID = as.numeric(ref.site$identifiers$id[ref.site$identifiers$idType=="WBAN"]),
            #                                 TimeBgn = bgn_temp,
            #                                 TimeEnd = end_temp,
            #                                 saveDir = save.dir)

            ref.data$Date=as.POSIXct(paste0(ref.data$UTC_DATE, ref.data$UTC_TIME), format="%Y%m%d%H%M", tz="UTC")

            if(nchar(as.character(ref.data$Date[1]))>10){
                frst30=grep(ref.data$Date[1:16], pattern = ":30:")
                ref.data=ref.data[-(1:(frst30-1)),]
            }


            ## Make 30 min direction data from refs
            ref.data=data.frame(ref.data %>%
                                    dplyr::group_by(Date = cut(Date, breaks="30 min")) %>%
                                    dplyr::summarize(airTemp = mean(as.numeric(AIR_TEMPERATURE))))

            neon.temp=Noble::pull.data(site = site, dp.id = "DP1.00002.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package = "basic", save.dir = save.dir)

            neon.data=data.frame(Date=as.POSIXct(neon.temp$startDateTime, tz = "UTC"), neon.temp=neon.temp$tempSingleMean.000.020)


            ref.data$Date=as.POSIXct(ref.data$Date, tz="UTC")
            #neon.data$Date=as.POSIXct(neon.data$Date, tz="UTC")


            comp.data=merge(x=neon.data, y=ref.data)

            correlation=stats::cor.test(x=comp.data$neon.temp, y=comp.data$airTemp, method = "spearman")
            p_val=correlation$p.value

            #p_val=NA
        }else if(ref.site$platform=="NRCS"){
            ref.data=RNRCS::grabNRCS.data(network = "SCAN",
                                          site_id = ref.site$identifiers$id[ref.site$identifiers$idType=="SCAN"],
                                          timescale = "hourly",
                                          DayBgn = bgn_temp,
                                          DayEnd = as.Date(end_temp))

            ref.data$Date=as.POSIXct(ref.data$Date, format="%Y-%m-%d %H:%M", tz=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site])

            simple.data=data.frame(
                Date=as.POSIXct(ref.data$Date, tz=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]),
                airTemp=(ref.data$Air.Temperature.Average..degF.-31)/1.8)


            simple.data$Date=as.POSIXct(format(simple.data$Date, tz="UTC", usetz = T), format="%Y-%m-%d %H:%M:%S")+
                lubridate::hours(as.POSIXct(bgn_temp, tz="UTC")-as.POSIXct(as.character(bgn_temp), tz=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]))
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
    message("External correlation testing completed")
    out=list("Internal Correlation"=sman.p, "External Correlation"=p_val)
    #browser()

    if(all(out$`Internal Correlation`$internal.p<.001, na.rm=T)){int.cor.result="Pass"}else{int.cor.result="Fail"}
    if(is.na(out$`External Correlation`)){ext.cor.result="No Test"}else if(out$`External Correlation`<0.001){ext.cor.result="Pass"}else{ext.cor.result="Fail"}
    if(any(c(drift.test, int.cor.result, ext.cor.result)=="Fail")){result="Fail"}else{result="Pass"}

    utils::write.csv(x = out$`External Correlation`, file = paste0(site.dir, "external_comparison.csv"), row.names = F)

    rslt.string=data.frame("Site"=site, "Begin.Month"=bgn.month, "end.month"=end.month, "Variance"=drift.test, "Internal.Correlation"=int.cor.result, "External.Correlation"=ext.cor.result, "Overall"=result)

    if(file.exists(.result.route(save.dir = save.dir))){
        prev=utils::read.csv(.result.route(save.dir = save.dir))
        curr=rbind(prev, rslt.string)
        utils::write.csv(x = curr, file = .result.route(save.dir = save.dir), row.names = FALSE)
    }else{
        utils::write.csv(x=rslt.string, file = .result.route(save.dir = save.dir), row.names = FALSE)
    }



    return(result)
}

