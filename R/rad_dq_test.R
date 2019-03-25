############################################################################################
#' @title  Execute Radiation Data Quality Testing

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Run 4 checks on radiation data quality for TIS sites: (1) Internal Correlation,
#' (2) Tower-Top Sensor Agreement, (3) Variance Stability, and
#' (4) External Correlation with a Non-NEON Site. Test results are written as simple pass/fail entries
#' in the results.csv files, however expanded test resullts are written in a site-specific folder in
#' the save directory.
#'
#' @param site The TIS site of interest, as a 4-letter code.
#' @param save.dir The save directory for data, results, and other files.
#' @param bgn.month The first month of testing, as "YYYY-MM".
#' @param end.month The last month of testing, as "YYYY-MM".
#'
#' @return Site results in 'results.csv', and raw

#' @keywords process quality, data quality, gaps, commissioning

#'


# changelog and author contributions / copyrights
#   Robert Lee (2018-01-03)
#     Re-wrote from earlier script (original creation)
#
##############################################################################################


## Function start
rad.dq.test=function(site, save.dir, bgn.month, end.month){

    startDateTime=NULL
    value=NULL
    variable=NULL
    gloRadMean.000.060=NULL
    UTC_DATE=NULL
    NEON.rad=NULL

    ########### GENERAL PARAMETERS ###########
    # if(!(site %in% Noble::rad_dq_info$Site)){
    #     message("Site is not in the current DQ test list. Please select from:")
    #     stop(paste0(Noble::rad_dq_info$Site))
    # }
    message(paste0("Testing ", site))

    #Define directories
    domn=Noble::is_site_config$domain[Noble::is_site_config$site.id==site]
    site.dir=Noble:::.data.route(site=site, save.dir=save.dir)

    rslt.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(rslt.dir)){
        dir.create(rslt.dir)
    }

    raw.dir=paste0(site.dir, "/rawData/")
    if(!dir.exists(raw.dir)){
        dir.create(raw.dir)
    }

    #set up DP info
    test.dp.ids=c("DP1.00014.001",
                  "DP1.00023.001",
                  "DP1.00024.001",
                  "DP1.00066.001"
    )
    # Add in Primary Pyranometer if Core site
    if(Noble::tis_site_config$core.relocatable[Noble::tis_site_config$site.id==site]=="Core"){test.dp.ids=c(test.dp.ids, "DP1.00022.001")}

    # Pull and refine data to vairance fields only
    lapply(test.dp.ids, function(x)
        try(Noble::pull.data(site = site,
                             dp.id = x,
                             bgn.month = bgn.month,
                             end.month = end.month,
                             time.agr = 30,
                             package = "basic",
                             save.dir = site.dir)
        )
    ) %>%
        do.call(what = cbind) %>%
        as.data.frame() -> rad.data


    ########### VARIANCE TESTING ###########
    ######## ALL AVAILABLE RAD DATA ########
    time.indx=grep(x = colnames(rad.data), pattern = "time", ignore.case = T)
    extra.times=time.indx[time.indx>3]
    rad.data=rad.data[,-extra.times]

    empty.cols=which(colSums(is.na(rad.data))==nrow(rad.data))
    rad.data=rad.data[,-empty.cols]

    var.cols=grep(colnames(rad.data), pattern = "variance", ignore.case = T)

    pdf(file = paste0(raw.dir, site, "_var_plots.pdf"), width = 10, height = 7.5, paper = "USr")
    out=lapply(var.cols,
               function(c) .rad.var.test(single.var.data=rad.data[,c(1,c)],
                                         bgn.month=bgn.month,
                                         end.month=end.month,
                                         raw.dir=raw.dir,
                                         site=site))
    grDevices::dev.off()


    out %>% do.call(what = rbind) %>% data.frame() %>% `colnames<-`(value=c("stream", "slope")) -> stable.vars

    stable.vars=stable.vars[!is.na(stable.vars$slope),]
    stable.vars$slope=as.numeric(stable.vars$slope)
    # stable.vars$test0.1="Fail"
    # stable.vars$test0.01="Fail"
    # stable.vars$test0.001="Fail"
    #
    # stable.vars$test0.1[stable.vars$slope<0.1]="Pass"
    # stable.vars$test0.01[stable.vars$slope<0.01]="Pass"
    # stable.vars$test0.001[stable.vars$slope<0.001]="Pass"

    utils::write.csv(x = stable.vars, file = paste0(raw.dir, "variance_stats.csv"))


    #utils::write.csv(stable.vars, file = paste0(""))

    # f.test.result=out$f.test.result
    # var.data=out$var.data
}
############################################
#
#
#
#
#
#     ########### INTERNAL CONSISTENCY ###########
#     ########### PAR and QL PAR ONLY ############
#     site.MLs=1:Noble::tis_site_config$num.of.mls[Noble::tis_site_config$site.id==site]
#     if(Noble::rad_dq_info$classification[Noble::rad_dq_info$Site==site]=="forest"){rho.TH=.65}else{rho.TH=.9}
#
#     PAR.pairwise=lapply(site.MLs[-1], function(x) c(x-1, x))
#     QL.PAR.pairwise=list(c(1,2), c(2,3))
#
#     PAR=try(Noble::pull.data(site = site,
#                              dp.id = "DP1.00024.001",
#                              bgn.month = bgn.month,
#                              end.month = end.month,
#                              time.agr = 30,
#                              package = "basic",
#                              save.dir = site.dir)
#     )
#     QL.PAR=try(Noble::pull.data(site = site,
#                                 dp.id = "DP1.00066.001",
#                                 bgn.month = bgn.month,
#                                 end.month = end.month,
#                                 time.agr = 30,
#                                 package = "basic",
#                                 save.dir = site.dir)
#     )
#
#     PAR=PAR[,grepl(pattern = "^PARMean", x = colnames(PAR))]
#     QL.PAR=QL.PAR[,grepl(pattern = "linePARMean", x = colnames(QL.PAR))]
#
#     PAR.rho=unlist(lapply(PAR.pairwise, function(x) round(stats::cor.test(PAR[,x[1]], PAR[,x[2]], method = "spearman", exact = F)$estimate, digits=2)))
#     QL.PAR.rho= unlist(lapply(QL.PAR.pairwise, function(x) round(stats::cor.test(QL.PAR[,x[1]], QL.PAR[,x[2]], method = "spearman", exact = F)$estimate, digits = 2)))
#
#     names(PAR.rho)=paste0("PAR-", PAR.pairwise)
#     names(QL.PAR.rho)=c("QL PAR 1-3", "QL PAR 3-5")
#
#     # One pair in each direction may fail
#     if(Noble::rad_dq_info$classification[Noble::rad_dq_info$Site==site]=="forest"){
#         if(length(PAR.rho<rho.TH)>=(length(PAR.rho)-1)){PAR.rho.test="Pass"}else{PAR.rho.test="Fail"} # ------>PAR rho results####
#         if(length(QL.PAR.rho<rho.TH)>=(length(PAR.rho)-1)){QL.PAR.rho.test="Pass"}else{QL.PAR.rho.test="Fail"} # ------>QL PAR rho results####
#     }
#
#     if(any(PAR.rho<rho.TH)==F){PAR.rho.test="Pass"}else{PAR.rho.test="Fail"} # ------>PAR rho results####
#     if(any(QL.PAR.rho<rho.TH)==F){QL.PAR.rho.test="Pass"}else{QL.PAR.rho.test="Fail"} # ------>QL PAR rho results####
#
#     ### Write stats out ###
#     raw.stats=data.frame(rho.estimate=append(PAR.rho, QL.PAR.rho))
#     utils::write.csv(x = raw.stats, file = paste0(raw.dir,"par_rho_stats.csv"), row.names = T)
#
#
#     ### Final Results ###
#     if(PAR.rho.test=="Fail"|QL.PAR.rho.test=="Fail"){
#         internal.compair.result="Fail"
#     }else{internal.compair.result="Pass"}
#
#     message(
#         paste0("PAR/QL PAR Internal Comparison Test: ", internal.compair.result) ###############################################################################################
#     )
#
#     ########### TOWER-TOP CONSISTENCY ###########
#     ########### Direct & Diffuse ONLY ###########
#
#     DirDif=try(Noble::pull.data(site = site,
#                                 dp.id = "DP1.00014.001",
#                                 bgn.month = bgn.month,
#                                 end.month = end.month,
#                                 time.agr = 30,
#                                 package = "basic",
#                                 save.dir = .data.route(site, save.dir = save.dir))
#     )
#
#     DirDif=data.frame(startDateTime=DirDif[,1],
#                       dirRadMean=DirDif[,grepl(colnames(DirDif), pattern = "dirRadMean")],
#                       difRadMean=DirDif[,grepl(colnames(DirDif), pattern = "difRadMean")],
#                       gloRadMean=DirDif[,grepl(colnames(DirDif), pattern = "gloRadMean")]
#     )
#
#
#
#     # Convert to local time
#     time.zone=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]
#     DirDif$startDateTime=as.POSIXct(DirDif$startDateTime, tz="UTC")
#     DirDif$startDateTime=as.POSIXct(format(DirDif$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)
#
#     #Only rows with greater than 5 W/m^2 get tested
#
#
# DirDif$SZA=GeoLight::zenith(GeoLight::solar(tm=DirDif$startDateTime),
#                  lon = Noble::tis_site_config$longitude[Noble::tis_site_config$site.id==site],
#                  lat = Noble::tis_site_config$latitude[Noble::tis_site_config$site.id==site]
#                  )
#
#     # DirDif$SZA=RAtmosphere::SZA(timein = DirDif$startDateTime,
#     #                             Lat = Noble::tis_site_config$Latitude[Noble::tis_site_config$site.id==site],
#     #                             Lon = Noble::tis_site_config$Longitude[Noble::tis_site_config$site.id==site]
#     # )
#
#     # Sum up the direct and diffuse rad
#     DirDif$total=DirDif$dirRadMean*cos(DirDif$SZA/180*pi)+DirDif$difRadMean
#     DirDif=DirDif[DirDif$total>5&!is.na(DirDif$total),]
#
#     # A. Ratio is within ±8% for solar zenith angle < 75°
#     set1=DirDif[which(DirDif$SZA<75),]
#     ratio1=round(sum(set1$total)/sum(set1$gloRadMean), 2)
#     if(0.92<ratio1&ratio1<1.08){
#         rat1Result="Pass"
#     }else(rat1Result="Fail")
#
#     # B. Ratio is within ±15% for 93° > solar zenith angle > 75°
#     set2=DirDif[which(DirDif$SZA>75&&DirDif$SZA<93),]
#     ratio2="NA"
#     rat2Result="NA"
#     if(length(set2[,1]>0)){
#         ratio2=round(sum(set2$total)/sum(set2$gloRadMean), 2)
#         if(0.85<ratio1&ratio1<1.15){
#             rat2Result="Pass"
#         }else(rat2Result="Fail")
#     }
#     if(rat2Result=="Fail"|rat1Result=="Fail"){
#         ratio.result="Fail"
#     }else{ratio.result="Pass"}
#
#     ### Raw data out
#     tower.top=data.frame(sets=c("SZA<75", "75<SZA<93"), ratio=c(ratio1, ratio2), result=c(rat1Result, rat2Result))
#     utils::write.csv(x = tower.top, file = paste0(raw.dir,"gloRad_tower_top_comparison.csv"), row.names = F)
#
#     message(
#         paste0("Tower Top (Direct + Diffuse, Global) Consistency Test: ", ratio.result) ##############################################################################################################
#     )
#
#     ########### GLOBAL RAD STREAMS AT TOWER TOP ###########
#
#     t.top.IDs=test.dp.ids[test.dp.ids %in% c("DP1.00014.001", "DP1.00022.001", "DP1.00023.001")]
#     raw.ttop.data=lapply(t.top.IDs, function(x)
#         try(Noble::pull.data(site = site,
#                              dp.id = x,
#                              bgn.month = bgn.month,
#                              end.month = end.month,
#                              time.agr = 30,
#                              package = "basic",
#                              save.dir = site.dir)
#         )
#     )
#
#     for(i in 1:length(raw.ttop.data)){
#         raw.ttop.data[[i]]$startDateTime=as.POSIXct(raw.ttop.data[[i]]$startDateTime, tz="UTC")
#     }
#
#     big.df=data.frame(
#         startDateTime=as.POSIXct(
#             Noble::help.time.seq(
#                 from = as.POSIXct(paste0(bgn.month, "-01"), tz = "UTC"),
#                 to=as.POSIXct(last.day.time(end.month = end.month, time.agr = 30), tz = "UTC"),
#                 time.agr = 30), tz="UTC"))
#
#     big.df$startDateTime=as.POSIXct(big.df$startDateTime, tz="UTC")
#
#     for(i in 1:length(raw.ttop.data)){
#         big.df=merge(x=big.df, y=raw.ttop.data[[i]])
#     }
#
#     big.df=big.df[,-which(grepl(colnames(big.df), pattern = ".000$"))] #Get rid of soil plots
#
#     data.indx=unlist(lapply(c("gloRadMean", "inSWMean", "shortRadMean"), function(x) which(grepl(pattern = x, x=colnames(big.df)))))
#
#
#
#     if(length(data.indx)==3){
#         list=list(
#             pair1=stats::cor.test(x=big.df[,data.indx[1]], y=big.df[,data.indx[2]], method = "spearman", conf.level = 0.95, exact = TRUE),
#             pair2=stats::cor.test(x=big.df[,data.indx[2]], y=big.df[,data.indx[3]], method = "spearman", conf.level = 0.95, exact = TRUE),
#             pair3=stats::cor.test(x=big.df[,data.indx[1]], y=big.df[,data.indx[3]], method = "spearman", conf.level = 0.95, exact = TRUE)
#         )
#
#         tower.top.glo.rad="Fail"
#         p.vals=lapply(list, "[[", "p.value")
#         probs=lapply(p.vals, function(x) 100-x)
#         if(isTRUE(unlist(lapply(list, "[[", "estimate")>.9))){tower.top.glo.rad="Pass"}
#
#     }
#     if(length(data.indx)==2){
#         list=stats::cor.test(x=big.df[,data.indx[1]], y=big.df[,data.indx[2]], method = "spearman", conf.level = 0.95)
#
#         tower.top.glo.rad="Fail"
#         if(list$estimate>=.9){tower.top.glo.rad="Pass"}
#
#     }
#
#     utils::write.csv(x = data.frame(unlist(list)), file = paste0(raw.dir, "tower_top_ratios.csv"), row.names = T)
#     message(paste0("Tower Top (Global Radiation Measurements) Consistency Test: ", tower.top.glo.rad))
#
#     # if(site %in% Noble::tis_site_config$site.id[Noble::tis_site_config$core.relocatable=="Core"]){
#     #
#     # }
#
#     ########### EXTERNAL CONSISTENCY ###########
#     ######## ALL GLORAD DATA ########
#
#     if(site=="BART"){
#         time.zone=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]
#
#         offset=difftime(time1 = as.POSIXct("2018-01-01", tz=time.zone), time2=as.POSIXct("2018-01-01", tz="UTC"))
#
#         temp=RNRCS::grabNRCS.data(network = "SCAN",
#                                   site_id = 2069,
#                                   timescale = "hourly",
#                                   DayBgn =paste0(bgn.month, "-01"),
#                                   DayEnd =stringr::str_sub(string =  as.character(Noble::last.day.time(end.month = end.month, time.agr = 1)), start = 1, end = 10)
#         )
#         ext.data=temp
#         ext.data$Date=as.POSIXct(ext.data$Date, tz=time.zone)
#
#         int.data=try(Noble::pull.data(site = site,
#                                       dp.id = "DP1.00014.001",
#                                       bgn.month = bgn.month,
#                                       end.month = end.month,
#                                       time.agr = 30,
#                                       package = "basic",
#                                       save.dir = .data.route(site, save.dir = save.dir))
#         )
#         int.data$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")#-lubridate::hours(offset+2)
#
#         int.rad<-data.frame(int.data %>%
#                                 dplyr::group_by(startDateTime = cut(startDateTime, breaks="60 min")) %>%
#                                 dplyr::summarize(gloRadMean.000.060 = mean(gloRadMean.000.060)))
#         int.rad$startDateTime=as.POSIXct(int.rad$startDateTime, tz="UTC")
#
#         all.data=merge(x=int.rad, y=ext.data, by.y = "Date", by.x = "startDateTime")
#
#         #qplot(x=all.data$Solar.Radiation.Average..watt.m2., y=all.data$gloRadMean.000.060)
#         ext.consist=stats::cor.test(x=all.data$Solar.Radiation.Average..watt.m2., y = all.data$gloRadMean.000.060, conf.level = 0.95, method = "spearman", exact = F)$estimate
#
#     #}
#     }else if(!is.na(Noble::rad_dq_info$nearestUSCRN[Noble::rad_dq_info$Site==site])){
#
#         uscrn.site=as.character(Noble::rad_dq_info$nearestUSCRN[Noble::rad_dq_info$Site==site])
#         uscrn.save.loc=paste0(.data.route(site = site, save.dir = save.dir), "/",uscrn.site, "_", bgn.month, "-", end.month, ".csv")
#         if(!file.exists(uscrn.save.loc)){
#             #browser()
#         temp=metget::getUSCRNData(temp_agg = "subhourly",
#                                     sid = uscrn.site,
#                                     start_date =  paste0(bgn.month, "-01"),
#                                     end_date =  as.character(Noble::last.day.time(end.month = end.month, time.agr = 1))
#         )
#         utils::write.csv(x=temp, file = uscrn.save.loc, row.names = F)
#         }else{
#             temp=utils::read.csv(file=uscrn.save.loc, stringsAsFactors = F, colClasses = "character")
#         }
#
#
#
#
#         ext.data=temp
#         ext.data$UTC_DATE=as.POSIXct(paste0(ext.data$UTC_DATE, ext.data$UTC_TIME), format="%Y%m%d%H%M", tz="UTC")
#         #utils::write.csv(x = ext.data, file = paste0(raw.dir, "USCRN_", uscrn.site, ".csv"), row.names = F)
#
#
#         ext.rad=data.frame(UTC_DATE=ext.data$UTC_DATE, USCRN.rad=ext.data$SOLAR_RADIATION)
#         int.data=try(Noble::pull.data(site = site,
#                                       dp.id = "DP1.00014.001",
#                                       bgn.month = bgn.month,
#                                       end.month = end.month,
#                                       time.agr = 1,
#                                       package = "basic",
#                                       save.dir = .data.route(site, save.dir = save.dir))
#         )
#         int.data$startDateTime=as.POSIXct(int.data$startDateTime, tz="UTC")
#         int.rad=data.frame(UTC_DATE=as.POSIXct(int.data$endDateTime, format="%Y-%m-%dT%H:%M:%SZ"), NEON.rad=int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")])
#         int.rad=data.frame(int.rad[-(1:4),] %>%
#                                dplyr::group_by(UTC_DATE = cut(UTC_DATE, breaks="5 min")) %>%
#                                dplyr::summarize(NEON.rad = mean(NEON.rad)))
#         int.rad$UTC_DATE=as.POSIXct(int.rad$UTC_DATE, tz="UTC")
#
#         ext.rad$UTC_DATE=as.POSIXct(ext.rad$UTC_DATE, tz="UTC")
#
#         all.data=merge(x=int.rad, y=ext.rad, by= "UTC_DATE")
#         all.data=all.data[all.data$USCRN.rad>-5,]
#
#
#
#         #bothRad<-data.frame(cbind(extRad, int.data[,grepl(x = colnames(int.data), pattern = "gloRadMean")]))
# #browser()
#         spearman.results=stats::cor.test(all.data$NEON.rad, as.numeric(all.data$USCRN.rad), method = "spearman", conf.level = 0.95, exact = F)
#
#         ext.consist=spearman.results$estimate
#
#         corr.plot=ggplot2::qplot(x=all.data$NEON.rad, y=all.data$USCRN.rad)+
#             ggplot2::ggtitle(label = paste0(site, "-", uscrn.site, " Rho: ", round(ext.consist, digits = 2)))+
#             ggplot2::geom_abline(slope = 1, color='red')+
#             ggplot2::coord_fixed()
#         ggplot2::ggsave(filename = paste0(raw.dir, "raw_corr_plot.pdf"), plot = corr.plot, device = "pdf", width = 7.5, height = 10, units = "in", dpi = 300)
#
#         utils::write.csv(x = data.frame(unlist(spearman.results)), file = paste0(raw.dir, "external_comparison.csv"), row.names = T)
#     }else{ext.consist=NA}
#     if(is.na(ext.consist)){external.test="No Test"}else if(ext.consist>0.90){external.test="Pass"}else{external.test="Fail"}
#     message(
#         paste0("External Consistency Test: ", external.test)
#     )
#
#     dq.rslt=data.frame(site=site,
#                        begin.month=bgn.month,
#                        end.month=end.month,
#                        variance.stability = f.test.result,
#                        internal.consistency=internal.compair.result,
#                        tower.top.consistency=tower.top.glo.rad,
#                        dir.dif.glo.consistency=ratio.result,
#                        external.consistency=external.test,
#                        test.date=as.character(Sys.Date()))
#
#     ########### WRITE TO RESULTS FILE ###########
#
#     if(file.exists(paste(rslt.dir,"results.csv",sep = "/"))){
#         dq.rpt <- utils::read.csv(file = paste(rslt.dir,"results.csv",sep = "/"), header = T, stringsAsFactors = T)
#         dq.rpt <- rbind(dq.rpt, dq.rslt)
#         utils::write.csv(x = dq.rpt, file = paste(rslt.dir,"results.csv",sep = "/"), row.names = F)
#     }
#     else{
#         utils::write.csv(x = dq.rslt, file = paste(rslt.dir,"results.csv",sep = "/"), col.names = T, row.names = F)
#     }
#
# }
