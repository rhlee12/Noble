############################################################################################
#' @title Produce a Summary Table of Data Product Health by Month

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce a data frame of data
#' product availability and validity for their period of record at a site.
#'
#'
#' Because the full period of record for all sites are queried,
#' this function can take a long time to execute.
#' @inheritParams dp.survey
#'
#' @param dp.id Parameter of class character. The NEON data product code of the data product of
#' interest.
#' @param site Parameter of class character. The NEON site of interest.
#'
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param save.dir The directory for data files to be saved to.


#' @return A data frame of health statisitcs by month for a given site. Raw NEON data are also saved to the specified save.dir, if supplied.

#' @keywords process quality, data quality, gaps, commissioning, data product, health

#' @examples
#' # Summarize 2D wind perfomance at CPER:
#' CPER_wind=dp.survey(dp.id = "DP1.00001.001", site="CPER")


#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-11-21)
#     original creation
#
##############################################################################################

health.data= function(site, dp.id, bgn.month, end.month, save.dir){

if(missing(save.dir)){save.dir=tempdir()}

    pri.var=Noble::tis_pri_vars$data.field[which(Noble::tis_pri_vars$dp.id==dp.id)]
    var.name=gsub(pattern = "mean", replacement = "", x = pri.var, ignore.case = T)

    dp.avail = neon.avail(dp.id = dp.id)
    dp.avail = cbind(Month=dp.avail[,1],  dp.avail[,which(colnames(dp.avail) %in% Noble::tis_site_config$site.id)])

    temp.dates = zoo::as.Date(dp.avail$Month[
        which(
            dp.avail[which(colnames(dp.avail)==site)]=="x"
        )
        ]
    )

    if(missing(bgn.month)&missing(end.month)){
        run.dates = substr(temp.dates, start = 0, stop = 7)
        info.dates=run.dates
    }else if(missing(end.month)){
        end.month=Sys.Date()
        info.dates=seq.Date(from=as.Date(paste0(bgn.month, "-01")), to=end.month, by="1 month")
        run.dates=base::substr(temp.dates[temp.dates %in% info.dates], start = 0, stop = 7)
    }else if(missing(bgn.month)){
        info.dates=seq.Date(from=as.Date("2014-01-01"), to=end.month, by="1 month")
        run.dates=substr(temp.dates[temp.dates %in% info.dates], start = 0, stop = 7)
    }else{
        info.dates=seq.Date(from=as.Date(paste0(bgn.month, "-01")), to=as.Date(paste0(end.month, "-01")), by="1 month")
        run.dates=substr(temp.dates[temp.dates %in% info.dates], start = 0, stop = 7)
    }

    health.data=data.frame(Month=substr(info.dates,0, 7), Availability=rep(0, times=length(info.dates)), Validity =rep(0, times=length(info.dates)))

    message(paste0("Working on ", site, "..."))
if(length(run.dates)>0){
    for(d in 1:length(run.dates)){
        message(paste0("Downloading ", run.dates[d]))
        month.data<-try(Noble::pull.data(site = site, dp.id = dp.id, bgn.month = run.dates[d], end.month = run.dates[d], time.agr = 30, package = "basic", save.dir = save.dir))

        if(!length(month.data)==0){
            priData=data.frame(month.data[,grepl(pattern = pri.var, x = colnames(month.data), ignore.case = T)])
            if(!length(priData)==0){

                pcntData=round(sum(colSums(!is.na(priData)))/(length(priData[,1])*length(priData))*100, digits = 2)

                finalQFs=data.frame(month.data[,grepl(pattern = "*finalQF*", x = colnames(month.data), ignore.case = T)])

                if(length(colnames(finalQFs))>length(colnames(priData))){
                    finalQFs=data.frame(finalQFs[,grepl(pattern = var.name, x = colnames(finalQFs), ignore.case = T)])
                }

                pcntValid=round(100-(sum(colSums(finalQFs, na.rm = T))/(length(priData[,1])*length(priData))*100+
                                         sum(colSums(is.na(finalQFs)))/(length(priData[,1])*length(finalQFs))*100), digits = 2)


                health.data$Availability[which(health.data$Month==run.dates[d])]=pcntData
                health.data$Validity[which(health.data$Month==run.dates[d])]=pcntValid}
        }
    }}
    return(health.data)
}
