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
#' bgn.month = "2017-09"
#' end.month = "2017-10"
#' time.agr = 30
#' package="basic"
#' save.dir<-tempdir()
#' Noble::tis.pq.test(site = site, bgn.month = bgn.month, end.month = end.month, save.dir=save.dir)
#' }
#'
#' @export


# changelog and author contributions / copyrights
#   Robert Lee (2018-03-25)
#     Original creation
#
##############################################################################################

soni.pq.test=function(site,  bgn.month, end.month, save.dir, q.th=95, v.th=90, overwrite=FALSE){
    time.agr = 30

    files=Noble::pull.eddy.data(site, bgn.month, end.month, package="expanded", save.dir)
    file.dir=.data.route(site = site, save.dir = save.dir)

    soni.pq=Noble::hdf5.to.df(site=site,
                              files = files,
                              data.type = "data",
                              var.name = "veloXaxsYaxsErth",
                              meas.name="soni",
                              bgn.month = bgn.month,
                              end.month = end.month,
                              time.agr = time.agr,
                              save.dir = file.dir,
                              overwrite = overwrite)

    soni.qm=Noble::hdf5.to.df(site=site,
                              files = files,
                              data.type = "qfqm",
                              var.name = "veloXaxsYaxsErth",
                              meas.name="soni",
                              bgn.month = bgn.month,
                              end.month = end.month,
                              time.agr = time.agr,
                              save.dir = file.dir,
                              overwrite = overwrite)
    ######

    soni.avail=round(sum(!is.na(soni.pq$mean))/nrow(soni.pq)*100, digits=2)
    soni.valid=round(sum(soni.qm$qfFinl==0)/nrow(soni.pq)*100, digits=2)

    bgn.day=as.Date(paste0(bgn.month, "-01"))
    end.day=as.POSIXct(Noble::last.day.time(end.month = end.month, time.agr = 1440))
    days=round(difftime(end.day, bgn.day, units="days"), digits = 2)


    ##### WRITE RESULTS
    dq.rslt<-data.frame(site=site,
                        time_performed=as.character(Sys.time()),
                        begin_month=bgn.month,
                        end_month=end.month,
                        days_tested=days,
                        data_product="DP4.00200.001",
                        variable_tested="mean.velo*axsErth",
                        data_quantity=soni.avail,
                        data_validity=soni.valid,
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
