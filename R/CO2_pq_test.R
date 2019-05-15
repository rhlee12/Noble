############################################################################################
#' @title  Perform A Commissioning PQ Test on IRGA CO2 Concentration Data

#' @author Robert Lee

#' @description For the specified dates, site, variables, and data product or name of family of data
#' products, data are downloaded and saved to the specifed directory. Process quality calculations are
#'  then performed and written to a results file in save.dir.
#'
#' @param site Parameter of class character. The NEON site data should be downloaded for.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first
#'  month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last
#'  month to get data for.
#' @param save.dir Parameter of class character. The local directory where data files should
#' be saved.
#' @param q.th Optional, the quanitity threshold used in testing. Defaults to 95
#' (95 percent of expected data must exist to pass the test)
#' @param v.th Optional, the quanitity threshold used in testing. Defaults to 88.1
#' (88.1 percent of expected data must exist and be unflagged to pass the test)
#'
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning
#'
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2018-03-25)
#     Original creation
#
##############################################################################################

co2.pq.test=function(site = site,  bgn.month, end.month, save.dir, q.th=90, v.th=80, overwrite=FALSE){
    time.agr = 30

    files=Noble::pull.eddy.data(site, bgn.month, end.month, package="expanded", save.dir)
    file.dir=.data.route(site = site, save.dir = save.dir)

    ### QF RESULTS (CORRECTED DATA ONLY)
    # FLATTEN HDF5
    co2.qm=Noble::hdf5.to.df(site=site,
                             files = files,
                             data.type = "qfqm",
                             var.name = "rtioMoleDryCo2",
                             meas.name="co2Turb",
                             bgn.month = bgn.month,
                             end.month = end.month,
                             time.agr = time.agr,
                             save.dir = file.dir,
                             overwrite = overwrite)
    # GENERATE STATISTIC
    co2.valid=round(sum(co2.qm$qfFinl==0, na.rm = TRUE)/nrow(co2.qm)*100, digits=2)

    ### CORRECTED RESULTS
    # FLATTEN HDF5
    co2.cor=Noble::hdf5.to.df(site=site,
                              files = files,
                              data.type = "data",
                              var.name = "rtioMoleDryCo2Cor",
                              meas.name="co2Turb",
                              bgn.month = bgn.month,
                              end.month = end.month,
                              time.agr = time.agr,
                              save.dir = file.dir,
                              overwrite = overwrite)
    # GENERATE STATISTIC
    cor.uptime=round(sum(!is.na(co2.cor$mean))/nrow(co2.cor)*100, digits=2)

    ### RAW (UNCORRECTED) RESULTS
    # FLATTEN HDF5
    co2.raw=Noble::hdf5.to.df(site=site,
                              files = files,
                              data.type = "data",
                              var.name = "rtioMoleDryCo2Raw",
                              meas.name="co2Turb",
                              bgn.month = bgn.month,
                              end.month = end.month,
                              time.agr = time.agr,
                              save.dir = file.dir,
                              overwrite = overwrite)
    # GENERATE STATISTIC
    raw.uptime=round(sum(!is.na(co2.raw$mean))/nrow(co2.raw)*100, digits=2)
    print(paste(site, nrow(co2.raw)))

    ### TOTAL TIME TESTED
    bgn.day=as.Date(paste0(bgn.month, "-01"))
    end.day=as.POSIXct(Noble::last.day.time(end.month = end.month, time.agr = 1440))
    days=round(difftime(end.day, bgn.day, units="days"), digits = 2)

    ### WRITE RESULTS
    # Make results DF
    dq.rslt<-data.frame(site=rep(site, 2),
                        time_performed=rep(as.character(Sys.time()),2),
                        begin_month=rep(bgn.month, 2),
                        end_month=rep(end.month,2),
                        days_tested=rep(days,2),
                        data_product="DP4.00200.001",
                        variable_tested=c("rtioMoleDryCo2Raw", "rtioMoleDryCo2Cor"),
                        data_quantity=c(raw.uptime, cor.uptime),
                        data_validity=c(co2.valid, "NA"),
                        quant_threshold= rep(q.th,2),
                        valid_threshold=rep(v.th,2)
    )

    # Append or creat results file as needed
    if(file.exists(.result.route(save.dir))){
        dq.rpt <- data.frame(utils::read.csv(file = .result.route(save.dir), header = T, stringsAsFactors = T))
        dq.rpt <- rbind(dq.rpt, dq.rslt)
        utils::write.csv(x = dq.rpt, file = .result.route(save.dir), row.names = F)
    }else{
        utils::write.csv(x = dq.rslt, file = .result.route(save.dir), col.names = T, row.names = F)
    }
    rhdf5::h5closeAll()
}



