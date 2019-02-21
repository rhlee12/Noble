############################################################################################
#' @title  Convert NEON Eddy Covaraince Data From hdf5 to Data Frames

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description This function will extract a given dataset ('meas.name') from the nested hdf5
#' data structure, and convert it to a data frame. If a save location is specified, a csv of the
#' data will also be saved.
#'
#' @param site Parameter of class character. The 4-letter NEON site code that the data is for.
#' @param hdf5.file Parameter of class character. The path to the hdf5 file to convert.
#' @param meas.name Parameter of class character. The name of the measurement in the hdf5 file
#' to be extracted.
#' @param time.agr What the time difference between sequence values should be, in minutes.
#' @param save.dir Optional. If specified a CSV of the extracted data will be saved to the
#' input directory.
#'
#' @return A data table of mesurements for the requested data product.
#'
#' @keywords eddy covariance, hdf5, process quality, data quality, gaps, commissioning

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2018-03-21)
#     original creation
#
##############################################################################################

hdf5.to.df=function(site, hdf5.file, meas.name, time.agr, save.dir){

    ok.time=c(1, 30)

    ok.meas=c("co2Stor","fluxHeatSoil", "h2oSoilVol", "h2oStor", "irgaCo2",
              "irgaH2o", "isoCo2", "isoH2o", "presBaro", "radiNet", "soni",
              "soniAmrs", "tempAirLvl", "tempAirTop", "tempSoil", "co2Turb", "amrs", "co2Stor")
    if(!meas.name %in% ok.meas){
        message("Invalid measurement name selected. Please enter one of the following:")
        stop(print(ok.meas))
    }
    if(!time.agr %in% ok.time){
        stop("Invalid temporal aggregation input. Please enter either 1 or 30.")
    }

    temp=rhdf5::h5read(file=hdf5.file, name=site)
    top.data=temp$dp01$data[names(temp$dp01$data)==meas.name][[1]]
    data=(top.data[grepl(x = names(top.data), pattern = paste0("*",time.agr,"m"))][[1]])
    for(d in 1:length(names(data))){
       colnames(data[[d]])[!grepl(x = colnames(data[[d]]), pattern = "time", ignore.case = T)]=paste0(colnames(data[[d]])[!grepl(x = colnames(data[[d]]), pattern = "time", ignore.case = T)], ".", names(data)[d])
    }
    qf.top=temp$dp01$qfqm[names(temp$dp01$data)==meas.name][[1]]
    qf=qf.top[grepl(x = names(qf.top), pattern = paste0("*",time.agr,"m"))][[1]]
    for(q in 1:length(names(qf))){
        colnames(qf[[q]])[!grepl(x = colnames(qf[[q]]), pattern = "time", ignore.case = T)]=paste0(colnames(qf[[q]])[!grepl(x = colnames(qf[[q]]), pattern = "time", ignore.case = T)], ".", names(qf)[q])
    }
    # uncert.top=temp$dp01$ucrt[names(temp$dp01$data)==meas.name][[1]]
    # unct=uncert.top[grepl(x = names(uncert.top), pattern = paste0("*",time.agr,"m"))][[1]]
    # for(u in 1:length(names(unct))){
    #     colnames(unct[[u]])[!grepl(x = colnames(unct[[u]]), pattern = "time", ignore.case = T)]=paste0(colnames(unct[[u]])[!grepl(x = colnames(unct[[u]]), pattern = "time", ignore.case = T)], ".", names(unct)[u])
    # }

    all=Reduce(function(x, y) merge(x, y, all=TRUE, by="timeBgn"), c(data, qf)) #, unct
    end.times=which(grepl(pattern = "timeEnd*", x = colnames(all)))
    all=all[-end.times]
    colnames(all)=gsub(x = colnames(all), pattern = "\\.y", replacement = "")
    colnames(all)=gsub(x = colnames(all), pattern = "\\.x", replacement = "")
    startDate=as.Date(all$timeBgn[1])
    endDate=as.Date(all$timeBgn[length(all$timeBgn)])

    if(!missing(save.dir)){
        if(dir.exists(save.dir)){
            utils::write.csv(x=all, file = paste0(.data.route(site = site, save.dir = save.dir), "/", meas.name, "_", site, "_", startDate, "-", endDate, ".csv"), row.names = F)
        }
        if(!dir.exists(save.dir)){
            warning("'save.dir' is not valid- no file saved.")
        }
    }
    return(all)
}
