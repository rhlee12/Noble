############################################################################################
#' @title Returns the end date and time within a specified month

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a temporal agregation, the function will return the latest date and time of that aggregation. For example, when December 2017 is specified with a 30 minute agregation, the function will return "2017-12-31 23:30:00".
#'
#' @param end.month Parameter of class character. The month in which the latest date and time should be returned.
#' @param time.agr What the temporal agregation of the returned time shoule be, in minutes.
#'
#' @return Latest date and time in the input month, offset from midnight by the input temporal agregation.
#'
#' @keywords process quality, data quality, gaps, commissioning

#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################
last.day.time<-function(end.month, time.agr){
    if(missing(time.agr)){
        message("Time agregation set to 1 second")
        time.agr=1/60
        }
    end_temp <- as.POSIXlt(paste0(end.month, "-01"), tz="UTC")
   # end_temp<- (paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end.date.time<-end_temp-lubridate::minutes(time.agr)
    return(end.date.time)
}
