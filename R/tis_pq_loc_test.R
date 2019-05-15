############################################################################################
#' @title  Downloads and performs process quality checks on NEON data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For the specified dates, site, variables, and data product or name of family of data products,
#' data are downloaded and saved to the specifed directory. Process quality calculations are then performed and written to a results file in save.dir.
#'
#' @param site Parameter of class character. The NEON site data should be downloaded for.
#' @param dp.id Parameter of class character. The name of the data product to pull data, or a
#' keyword for a family of data products, e.g. "wind" will pull for 2D and 3D wind data products.
#' @param prin.vars The principle variables to test (variable names, such as 'windSpeed'). Omit the term 'Mean'.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param time.agr Parameter of class numeric. The data agregation interval requested, must be 1, 2, or 30.
#' @param package Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param save.dir Parameter of class character. The local directory where data files should be saved.
#' @param q.th Parameter of class character. Optional. The threshold for data availability for a passing test, defaults to 0.95.
#' @param v.th Parameter of class character. Optional. The threshold for data validity for a passing test, defaults to 0.9.
#'
#' @return Writes data files to the specified directory.

#' @keywords process quality, data quality, gaps, commissioning

#'@export


# changelog and author contributions / copyrights
#   Robert Lee (2017-07-20)
#     original creation
#
##############################################################################################

tis.pq.loc.test<-function(site, dp.id,
                          prin.vars,  bgn.month,
                          end.month, save.dir,
                          time.agr = 30, package="basic",
                          q.th, v.th, exclude.dff=F){

    options(stringsAsFactors = FALSE)

    if(dp.id=="DP1.00001.001"&package=="basic"){
        message("2D wind speed PQ testing requires the expanded package.\nThe expanded package will be tested.")
        package="expanded"
    }

    if(missing(q.th)){
        q.th=90
        quant_threshold=q.th
    }
    if(missing(v.th)){
        v.th=85
        valid_threshold=v.th
    }
    quant_threshold=q.th
    valid_threshold=v.th


    #Make domain-specific directory
    site.dir=.data.route(site = site, save.dir = save.dir)

    #pull data
    test.data=data.frame()
    test.data=try(Noble::pull.data(site = site, dp.id = dp.id, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir = site.dir))
    if(length(test.data)>1){
        sensor.locs=as.character(na.exclude(unique(stringr::str_extract(colnames(test.data), pattern = "[0-9]{3}.[0-9]{3}"))))
        if(any(grepl(pattern = "inSW", x = prin.vars))){
            sensor.locs=sensor.locs[-grep(pattern = "003.000", x = sensor.locs, ignore.case = T)]
        }
        for(sl in 1:length(sensor.locs)){
            sensor.data=test.data[,c(1, 2, grep(pattern = sensor.locs[sl], x = colnames(test.data)))]
            for(i in 1:length(prin.vars)){
                data.indx<-grep(x=colnames(sensor.data), pattern=paste0("^", prin.vars[i], "Mean*"))

                qf.indx<-grep(x=colnames(sensor.data), pattern=paste0("^", prin.vars[i], "FinalQF*"))
                qf.indx<-append(qf.indx, grep(x=colnames(sensor.data), pattern="^finalQF*"))

                # if(prin.vars[i]=="inSW"){
                #     data.indx=data.indx[-which(grepl(x=colnames(sensor.data[,data.indx]), pattern = ))]
                #     qf.indx=qf.indx[-which(grepl(x=colnames(sensor.data[,qf.indx]), pattern = "003.000"))]
                # }
                #special case for precip
                if(prin.vars[i]=="priPrecipBulk"){
                    data.indx<-grep(x=colnames(sensor.data), pattern=paste0("^", prin.vars[i]))
                    qf.indx<-grep(x=colnames(sensor.data), pattern=("priPrecipFinalQF\\."), ignore.case = T)
                }

                bgn.day=as.Date(paste0(bgn.month, "-01"))
                end.day=as.POSIXct(Noble::last.day.time(end.month = end.month, time.agr = 1))#time.agr = 1440

                days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
                end.day=lubridate::round_date(end.day, "day")

                all.data=length(data.indx)*length(sensor.data[,1])

                num.nas<-sum(is.na(sensor.data[,data.indx]))
                num.data<-sum(!is.na(sensor.data[,data.indx]))

                data.quant<-round(100*(num.data/(all.data)), digits = 2)
                ### Validity/Flagging testing
                if(dp.id=="DP1.00001.001"&exclude.dff){
                    data.valid=.wind.validity(data.quant=data.quant, data=sensor.data) # testing to exclude the
                }else{
                num.qf.fail<-sum(sensor.data[,qf.indx]==1, na.rm=TRUE)
                num.qf.pass<-sum(sensor.data[,qf.indx]==0, na.rm = TRUE)
                num.qf.na<-sum(is.na(sensor.data[,qf.indx]))

                data.valid=round(100*(num.qf.pass/(all.data)), digits = 2)
                }
                if(prin.vars[i]=="SHF"&missing(q.th)&missing(v.th)){
                    #Soil heat flux specific values
                    quant_threshold=95
                    valid_threshold=(90-15.38)
                }
                if(prin.vars[i]=="soilTemp"&missing(q.th)&missing(v.th)){
                    quant_threshold=94.6
                    valid_threshold=89.87
                }

                pass.fail=if(data.quant>=quant_threshold&data.valid>=valid_threshold){"PASS"}else{"FAIL"}
                ##### WRITE RESULTS
                dq.rslt<-data.frame(site=site,
                                    time_performed=as.character(Sys.time()),
                                    begin_month=bgn.month,
                                    end_month=end.month,
                                    days_tested=days,
                                    data_product= dp.id,
                                    sensor_location=paste0("H.V: ", sensor.locs[sl]),
                                    variable_tested=prin.vars[i],
                                    data_quantity=data.quant,
                                    data_validity=data.valid,
                                    quant_threshold= quant_threshold,
                                    valid_threshold=valid_threshold,
                                    result=pass.fail
                )

                if(file.exists(.result.route(save.dir))){
                    dq.rpt <- data.frame(utils::read.csv(file = .result.route(save.dir), header = T, stringsAsFactors = F))
                    dq.rpt <- rbind(dq.rpt, dq.rslt)
                    utils::write.csv(x = dq.rpt, file = .result.route(save.dir), row.names = F)
                }
                else{
                    utils::write.csv(x = dq.rslt, file = .result.route(save.dir), row.names = F)
                }
            }
        }
    }else{
        for(i in 1:length(prin.vars)){
            if(prin.vars[i]=="SHF"&missing(q.th)&missing(v.th)){
                #Soil heat flux specific values
                quant_threshold=95
                valid_threshold=(90-15.38)
            }
            if(prin.vars[i]=="soilTemp"&missing(q.th)&missing(v.th)){
                quant_threshold=94.6
                valid_threshold=89.87
            }

            bgn.day=as.Date(paste0(bgn.month, "-01"))
            end.day=as.POSIXct(Noble::last.day.time(end.month = end.month, time.agr = 1440))

            days=round(difftime(end.day, bgn.day, units="days"), digits = 2)
            ##### WRITE RESULTS
            dq.rslt<-data.frame(site=site,
                                time_performed=as.character(Sys.time()),
                                begin_month=bgn.month,
                                end_month=end.month,
                                days_tested=days,
                                sensor_location="NA",
                                data_product= dp.id,
                                variable_tested=prin.vars[i],
                                data_quantity=0,
                                data_validity=0,
                                quant_threshold= quant_threshold,
                                valid_threshold=valid_threshold,
                                result="NA"
            )

            if(file.exists(.result.route(save.dir))){
                dq.rpt <- data.frame(utils::read.csv(file = .result.route(save.dir), header = T, stringsAsFactors = T))
                dq.rpt <- rbind(dq.rpt, dq.rslt)
                utils::write.csv(x = dq.rpt, file = .result.route(save.dir), row.names = F)
            }
            else{
                utils::write.csv(x = dq.rslt, file = .result.route(save.dir), row.names = F)
            }}
    }
}

