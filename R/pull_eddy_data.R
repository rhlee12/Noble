############################################################################################
#' @title  Download NEON Eddy Covaraince Data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description This function downloads HDF5 data from the NEON data portal to the specified save
#' directory. Files are not collated together by month currently, instead they are saved on a site and
#' month basis.
#'
#' @param site Parameter of class character. The 4-letter NEON site code that the data is for.
#' @param bgn.month Parameter of class character. The year-month (e.g. "2017-01") of the first month to get data for.
#' @param end.month Parameter of class character. The year-month (e.g. "2017-01") of the last month to get data for.
#' @param package Parameter of class character. Optional. The type of data package to be returned If not specified, defaults to basic.
#' @param save.dir Optional. If specified a CSV of the extracted data will be saved to the
#' input directory.
#'
#' @return HDF5 data files are saved to the specified directory.
#'
#' @keywords eddy covariance, hdf5, process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{

#' site="CPER"
#' bgn.month="2017-04"
#' end.month="2017-11"
#' package="basic"
#' save.dir=tempdir()

#' pull.eddy.data(site, bgn.month, end.month, package, save.dir)
#' }
#' @export
#'
# changelog and author contributions / copyrights
#   Robert Lee (2018-03-21)
#     original creation
#
##############################################################################################

# #test block
# test.dir="~/Desktop/"
# site="CPER"
# bgn.month="2017-04"
# end.month="2017-11"
# package="basic"
# save.dir=test.dir


pull.eddy.data=function(site, bgn.month, end.month, package, save.dir){
    download.file=NULL

    file.dir=.data.route(site = site, save.dir = save.dir)

    dp.data=jsonlite::read_json(path="http://data.neonscience.org/api/v0/products/DP4.00200.001/")$data
    deployed.sites=unlist(lapply(seq(length(dp.data$siteCodes)), function(x) dp.data$siteCodes[[x]]$siteCode))
    if(site %in% deployed.sites){
        avail.months=unlist(lapply(dp.data$siteCodes, function(x) if(x$siteCode==site){x$availableMonths}))
        avail.urls=unlist(lapply(dp.data$siteCodes, function(x) if(x$siteCode==site){x$availableDataUrls}))
    }else{stop(paste0("Bundled eddy covariance data products aren't available at ", site, " yet!"))}

    call.df=data.frame(avail.months=avail.months, avail.urls=avail.urls)

    requested.months=substr(seq.Date(from = as.Date(paste0(bgn.month, "-01")), to=as.Date(paste0(end.month, "-01")), by="month"), 1, 7)

    api.months=requested.months[requested.months %in% call.df$avail.months]
    if(length(api.months)==0){stop("No data found in specified date range.")}

    data.links=lapply(api.months, function(x) jsonlite::read_json(call.df$avail.urls[call.df$avail.months==x]))

    data.links=lapply(seq(length(data.links)), function(x) data.links[[x]]$data$files)

    hdf5.links=unlist(lapply(data.links, function(x) lapply(seq(length(x)), function(y) if(grepl(pattern = ".zip", x = x[[y]]$name)){x[[y]]$url})))#, function(y) )))

    hdf5.links=hdf5.links[grepl(pattern = package, x = hdf5.links)]

    for(i in 1:length(hdf5.links)){
        month=stringr::str_extract(hdf5.links[i],pattern = "\\d\\d\\d\\d-\\d\\d")
        file=paste0(file.dir,"/", site, "_", month,"_", package, "_DP4.00200.001.zip")
    }

    curr.files=list.files(file.dir, pattern = ".zip", full.names = T)

    file.dates=gsub(x = stringr::str_extract(string=hdf5.links, pattern = "\\.\\d{4}-\\d{2}"), pattern = "\\.", replacement = "")

    hdf5.info=data.frame(cbind(hdf5.links, file.dates))

    hdf5.info$zip=rep(NA, times=length(hdf5.info[,1]))

    for(x in 1:length(hdf5.info[,1])){
        zip.save=paste0(file.dir, site, "_", hdf5.info[x,2], "_DP4.00200.001.zip")
        hdf5.file=paste0(file.dir, site, "_", hdf5.info[x,2], "_DP4.00200.001.h5")
        if(!file.exists(paste0(file.dir,"/", site, "_", month,"_DP4.00200.001.zip"))){
            download.file(url = hdf5.info[x,1], destfile = zip.save)
        }
        utils::unzip(zipfile = zip.save, exdir = file.dir, overwrite = T)
    }
    found.h5s=list.files(file.dir)[grep(x = list.files(file.dir), pattern = "\\.h5")]
    out= found.h5s[which(stringr::str_extract("\\d{4}-\\d{2}", string=  found.h5s) %in% requested.months)]
    return(out)
}

