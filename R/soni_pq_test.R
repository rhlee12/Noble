############################################################################################
#' @title  Perform A Commissioning PQ Test on 3D Sonic Anemometer Data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, variables, and data product or name of family of data
#' products, data are downloaded and saved to the specifed directory. Process quality calculations are
#'  then performed and written to a results file in save.dir.
#' @inheritParams tis.pq.test
#'
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' site = "CPER"
#' bgn.month = "2017-05"
#' end.month = "2017-06"
#' time.agr = 30
#' package="basic"
#' save.dir<-tempdir()
#' Noble::tis.pq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)
#' }


# changelog and author contributions / copyrights
#   Robert Lee (2018-03-25)
#     Original creation
#
##############################################################################################

soni.pq.test=function(site = "CPER",  bgn.month, end.month, save.dir, q.th=95, v.th=90){
    time.agr = 30
    files=Noble::pull.eddy.data(site, bgn.month, end.month, package="basic", save.dir)
    file.dir=.data.route(site = site, save.dir = save.dir)
    soni.raw=lapply(files, function(x) Noble::hdf5.to.df(site, hdf5.file=paste0(file.dir,x), meas.name="soni", time.agr, save.dir))
    soni.df=do.call(rbind, soni.raw)
    soni.df$timeBgn=gsub(x = soni.df$timeBgn, pattern = "T", replacement = " ")
    soni.df$timeBgn=gsub(x = soni.df$timeBgn, pattern = "z", replacement = "")

    soni.df$timeBgn=as.POSIXct(soni.df$timeBgn, tz="UTC")
    soni.df$timeBgn=format(soni.df$timeBgn, tz="UTC")

    ref.times=data.frame(timeBgn=Noble::help.time.seq(from = as.Date(paste0(bgn.month, "-01")), to=Noble::last.day.time(end.month = end.month, time.agr = time.agr), time.agr = time.agr))
    ref.times$timeBgn=format(as.POSIXct(ref.times$timeBgn, tz = "UTC"), tz="UTC")

    test.data=merge(x=ref.times, y=soni.df, by="timeBgn", all.x = T)

    if(length(test.data)>1){
        data.indx<-grep(x=colnames(test.data), pattern="mean\\.velo.axsErth") #pattern="mean.veloZaxsErth"

        qf.indx<-grep(x=colnames(test.data), pattern="qfFinl.angZaxsErth")
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
                            variable_tested="mean.velo*axsErth",
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

