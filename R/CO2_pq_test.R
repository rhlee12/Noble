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

co2.pq.test=function(site = site,  bgn.month, end.month, save.dir, q.th=95, v.th=88.1){
    time.agr = 30
    files=Noble::pull.eddy.data(site, bgn.month, end.month, package="basic", save.dir)
    file.dir=.data.route(site = site, save.dir = save.dir)
    co2.raw=lapply(files, function(x) Noble::hdf5.to.df(site, hdf5.file=paste0(file.dir,x), meas.name="co2Turb", time.agr, save.dir))
    co2.df=do.call(rbind, co2.raw)
    co2.df$timeBgn=gsub(x = co2.df$timeBgn, pattern = "T", replacement = " ")
    co2.df$timeBgn=gsub(x = co2.df$timeBgn, pattern = "z", replacement = "")

    co2.df$timeBgn=as.POSIXct(co2.df$timeBgn, tz="UTC")
    co2.df$timeBgn=format(co2.df$timeBgn, tz="UTC")

    ref.times=data.frame(timeBgn=Noble::help.time.seq(from = as.Date(paste0(bgn.month, "-01")), to=Noble::last.day.time(end.month = end.month, time.agr = time.agr), time.agr = time.agr))
    ref.times$timeBgn=format(as.POSIXct(ref.times$timeBgn, tz = "UTC"), tz="UTC")

    test.data=merge(x=ref.times, y=co2.df, by="timeBgn", all.x = T)

    if(length(test.data)>1){
        data.indx<-grep(x=colnames(test.data), pattern="mean.densMoleCo2")

        qf.indx<-grep(x=colnames(test.data), pattern="qfFinl.densMoleCo2")
        #qf.indx<-append(qf.indx, grep(x=colnames(test.data), pattern="^finalQF*"))

        bgn.day=as.Date(paste0(bgn.month, "-01"))
        end.day=as.POSIXct(Noble::last.day.time(end.month = end.month, time.agr = 1440))

        days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
        end.day=lubridate::round_date(end.day, "day")

        all.data=length(data.indx)*length(test.data[,1])

        num.nas<-sum(is.na(test.data[,data.indx]))
        num.data<-sum(!is.na(test.data[,data.indx]))

        data.quant<-round(100*(num.data/(all.data)), digits = 2)

        num.qf.fail<-sum(test.data[,qf.indx]==1, na.rm=TRUE)
        num.qf.pass<-sum(test.data[,qf.indx]==0, na.rm = TRUE)
        num.qf.na<-sum(is.na(test.data[,qf.indx]))

        data.valid<-round(100*(num.qf.pass/(all.data)), digits = 2)

        ##### WRITE RESULTS
        dq.rslt<-data.frame(site=site,
                            time_performed=as.character(Sys.time()),
                            begin_month=bgn.month,
                            end_month=end.month,
                            days_tested=days,
                            data_product="DP4.00200.001",
                            variable_tested="densMoleCo2",
                            data_quantity=data.quant,
                            data_validity=data.valid,
                            quant_threshold= q.th,
                            valid_threshold=v.th
        )

        if(file.exists(.result.route(save.dir))){
            dq.rpt <- data.frame(utils::read.csv(file = .result.route(save.dir), header = T, stringsAsFactors = T))
            dq.rpt <- rbind(dq.rpt, dq.rslt)
            utils::write.csv(x = dq.rpt, file = .result.route(save.dir), row.names = F)
        }else{
            utils::write.csv(x = dq.rslt, file = .result.route(save.dir), col.names = T, row.names = F)
        }
    }
}

