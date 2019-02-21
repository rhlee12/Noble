############################################################################################
#' @title  Returns indicies for missing NEON data

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Given a data frame of NEON data, \code{gap.find} will return either a list rows or
#' list of timestamps. Each returned list is list of missing timestamps, missing data (all NAs),
#' and data with no values, but that are still quality flagged.\cr
#'
#' Whether rows of missing data or timestamps are returned depends on the return parameter.
#' If return = "index", the returned list components will be as follows:\cr
#' \cr
#'     miss.indx - The row numbers where timestamps and data were missing\cr
#'     no.data.indx - The row numbers where data are missing, but are quality flagged\cr
#'     no.qf.indx - The row numbers where all data and QFs were 'NA'\cr
#'
#' If return = "times", the returned list components will be as follows:\cr
#' \cr
#'     miss.times - The times where timestamps and data were missing\cr
#'     na.data.times - The times where all data and QFs were 'NA'\cr
#'     no.qf.times - The times where data are missing, but are quality flagged\cr
#'
#' @param data A data frame of NEON data returned from the API or SOM tool.
#' @param time.agr Optional, but recommended. The data aggregation period of the input data
#' (difference between timestamps in minutes), if not specified it will guess at the value.
#' @param return Optional. Used to specifiy whether row numbers ("index") or timestamps
#' ("times") are returned. Defaults to row numbers if not specified.

#' @return Outputs a list of lists of row numbers or timestamps where data is missing.
#'

#' @keywords process quality, data quality, gaps, commissioning

#' @export
# changelog and author contributions / copyrights
#   Robert Lee (2017-07-10)
#     original creation
#
# changelog and author contributions / copyrights
#   Robert Lee (2018-04-27)
#     Attempt to accomodate soil plot DPs.
#
##############################################################################################

gap.find<-function(data, time.agr, return){

    valid.returns <- c("index", "times")

    if(missing(return)){
        message("No return type specified, will return an index of rows with missing data")
        return<-"index"
    }
    if(!return %in% valid.returns){
        stop("Please specify either 'index' or 'times' for the return parameter")
    }

    #make indicies for columns of time, data, and quality flags
    time.indx <- grep(colnames(data), pattern = "time", ignore.case = T)
    data.indx <- grep(x=colnames(data), pattern = "data|mean", ignore.case = T)
    qf.indx <- grep(colnames(data), pattern = "qf", ignore.case = T)

    # Convert the first timestamp sequence found to POSIX
    data[, time.indx[1]]<-as.POSIXct(data[,time.indx[1]], format="%Y-%m-%d %H:%M:%S", tz="UTC")

    # If the time.agr parameter isn't specified, make a guess about the value
    if(missing(time.agr)){
        time.agr<-as.numeric(difftime(data[2,time.indx[1]], data[1,time.indx[1]], units = "mins"))
        message(paste0("No time.agr value specified, making an educated guess that it is: ", time.agr, " minutes."))
    }

    # Make a list of reference times, to check against
    frst.time<-as.POSIXct(data[1,time.indx[1]], tz="UTC")
    last.time<-as.POSIXct(data[length(data[,time.indx[1]]),time.indx[1]], tz="UTC")
    num.times <- as.numeric(difftime(time1 = last.time, time2 = frst.time, units = "mins"))/time.agr+1

    #Make a data frame with reference times
    ref.times.df<- data.frame( "index"= seq.int(from = 1, to = num.times), "ref.times"=seq.POSIXt(from=frst.time, to=last.time, by=time.agr*60))

    # Compare timestamps to posix sequence, Find indicies of POS.seq that are not in the data timestamps
    miss.indx <- ref.times.df$index[which(!as.POSIXct(ref.times.df$ref.times) %in% as.POSIXct(data[,time.indx[1]]))] #Missing times
    no.data.indx<- which(unlist(lapply(seq(from=1,to=length(data[,1]), by = 1), function(x) all(is.na(data[x,data.indx])))))
    no.qf.indx <- which(unlist(lapply(seq(from=1,to=length(data[,1]), by = 1), function(x) all(is.na(data[x,qf.indx]))))) #missing all data

    # Report the timestamps for missing data
    miss.times <- ref.times.df$ref.times[miss.indx]
    no.data.times <- data[no.data.indx,time.indx[1]] #[which(ref.times.df$index %in% na.time.indx)] #data[which(is.na(data[,data.indx])&data[,qf.indx]=="1"), time.indx[1]]
    no.qf.times <- data[no.qf.indx,time.indx[1]]

    #put everything into (named) lists
    gap.index<-list(miss.times, no.data.indx, no.qf.indx)
    names(gap.index)<-c("miss.times", "no.data.indx", "no.qf.indx")
    gap.times<-list(miss.times, no.data.times, no.qf.times)
    names(gap.times)<-c("miss.times", "no.data.times", "no.qf.times")

    #return the requested type of info
    if(return=="index"){
        return(gap.index)
    }
    if(return=="times"){
        return(gap.times)
    }

}
