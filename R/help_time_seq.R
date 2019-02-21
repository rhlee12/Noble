############################################################################################
#' @title  Generates a time sequence over the specified interval

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description A time sequence, with spacing between values given by the time.agr parameter
#' (eg. a time.agr value of 30 means there should be a 30 minute jump between times in the output sequence).
#'
#' @param from Parameter of class character. The start time of the sequence.
#' @param to Parameter of class character. The end time of the sequence.
#' @param time.agr What the time difference between sequence values should be, in minutes.
#'
#' @return A time sequence over the specified interval.
#'
#' @keywords process quality, data quality, gaps, commissioning
#'
#' @export

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-18)
#     original creation
#
##############################################################################################
help.time.seq<-function(from, to, time.agr)
{
    time.seq<-seq.POSIXt(from = as.POSIXct(from, tz="UTC"), to=as.POSIXct(to, tz="UTC"), by=(time.agr/60)*3600)
    return(time.seq)
}
