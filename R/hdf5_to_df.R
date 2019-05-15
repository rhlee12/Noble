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

hdf5.to.df=function(site, files, data.type, meas.name, var.name, bgn.month, end.month, time.agr, save.dir, overwrite=FALSE){
    library(magrittr)

    ### INPUT CHECKING
    ok.time=c(1, 30)

    ok.meas=c("amrs", "co2Stor", "co2Turb", "fluxHeatSoil", "h2oSoilVol", "h2oStor",
              "h2oTurb", "isoCo2", "isoH2o", "presBaro", "radiNet", "soni",
              "tempAirLvl", "tempAirTop", "tempSoil")
    #ok.vars=c()

    if(!meas.name %in% ok.meas){
        message("Invalid measurement name selected. Please enter one of the following:")
        stop(print(ok.meas))
    }
    if(!time.agr %in% ok.time){
        stop("Invalid temporal aggregation input. Please enter either 1 or 30.")
    }

    ### FILE NAME PARAMETERS
    start.date=paste0(bgn.month, "-01")
    end.date=Noble::last.day.time(end.month = end.month, time.agr = time.agr)

    file.out=paste0(save.dir, "/", "EC_", data.type,"_", meas.name, "_", var.name, "_", start.date, "-", substr(end.date, start = 1, stop = 10), ".csv")
    print(file.out)
    ### GENERATE NEW FLAT DF
    if(!file.exists(file.out)|all(file.exists(file.out), overwrite)){

        top.ml=Noble::tis_site_config$num.of.mls[Noble::tis_site_config$site.id==site]
        # GENERATE H.V.T GROUP MEETING

        hor.ver.tmi=paste0("000_0", top.ml, "0_", stringr::str_pad(string = time.agr, width = 2, side = "left", pad = "0"), "m")
troubleshoot=function(hdf5.file){
    print(hdf5.file)
    try(rhdf5::h5read(file=hdf5.file, paste0(site,'/dp01/', data.type, '/',meas.name,'/', hor.ver.tmi, "/", var.name)))
}
        ec.list=lapply(files, troubleshoot)

        ec.list=ec.list[lapply(ec.list, class)=="data.frame"]

        ec.data=do.call(plyr::rbind.fill, ec.list)
        clean.times=function(x){
            x %>%
                gsub(pattern = "T|Z", replacement = " ") %>%
                trimws() %>%
                as.POSIXct(tz = "UTC", format="%Y-%m-%d %H:%M:%S") -> out
            return(out)
        }
        ec.data$timeBgn=clean.times(ec.data$timeBgn)

        ref.seq=data.frame(startDateTime=Noble:::help.time.seq(from = start.date, to = end.date, time.agr = time.agr))
        out=merge(x=ref.seq, y = ec.data, by.x = "startDateTime", by.y = "timeBgn", all.x = TRUE)

        write.csv(x = out, file = file.out, row.names = FALSE)

    }else{
        out=read.csv(file.out, stringsAsFactors = FALSE)
    }
    rhdf5::h5closeAll()
    return(out)
}
