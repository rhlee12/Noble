############################################################################################
#' @title  Refine Data from Multiple Soil Plot Locations Down to a Specified Location

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For data downloaded by Noble, and a specified soil plot location, data only
#' from the location are returned as a data frame.
#'
#' @param data A data frame of instrumented pull.dataed with \code{pull.data}.
#' @param sp Desired soil plot, given as an integer (eg. sp=1, corresponding to the
#' first soil plot, closest to the tower)
#'
#' @return Returns a data frame of data just from the specified measurement location

#' @keywords process quality, data quality, gaps, commissioning
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2018-04-27)
#     original creation
#
##############################################################################################

sp.extract<-function(data, sp){
    #tis_site_config<-Noble::tis_site_config
    time.indx <- grep(x=colnames(data), pattern = "time", ignore.case = T)
    sp.pattern <- paste0("00", sp, ".5*")
    sp.data<-data[,which(grepl(colnames(data), pattern = sp.pattern))]
    sp.data<-cbind(data[,time.indx], sp.data)
    return(sp.data)
}
