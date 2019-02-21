############################################################################################
#' @title  Generate and Write Data Product Gap Summaries

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a specified data product ID, this function will produce summary CSVs of data
#' and quality flag gaps for their period of record at a site.
#'
#' @param site A NEON TIS site
#' @param dp.id Parameter of class character. The NEON data product code of the data product of interest.
#' @param bgn.month The fisrt month of data to examine for gaps.
#' @param end.month The last month of data to examine for gaps.
#' @param save.dir The directory to write data files to.


#' @return Writes two CSVs of the start end times of gaps, one CSV for data gaps, the other for quality flag gaps.

#' @keywords process quality, data quality, gaps, commissioning, data product, health

#' @examples
#' \dontrun{
#' # For 2d Wind, save files to the current working directory:
#' gap.report(site="CPER", dp.id = "DP1.00001.001",
#' bgn.month="2017-07", end.month="2017-07", save.dir = getwd())
#' }

#' @seealso gap.find
#' @export
#'
############################################################################################
gap.report=function(site, bgn.month, end.month, dp.id, save.dir){

    data=Noble::pull.data(dp.id = dp.id, site = site, bgn.month = bgn.month, end.month = end.month,
                          time.agr = 30, package = "basic", save.dir = tempdir())
    short.name=Noble::tis_pri_vars$short.name[Noble::tis_pri_vars$dp.id==dp.id]
    raw.mls=zoo::na.trim(stringr::str_extract(string = colnames(data), pattern = "\\.\\d\\d\\d\\.\\d\\d\\d"))
    tower.mls=as.numeric(unique(stringr::str_sub(raw.mls, start = 7, end = 7)))

    locs=as.character(stats::na.exclude(unique(stringr::str_extract(string = colnames(data), pattern = "\\d{3}\\.\\d{3}"))))

    sp.locs=unique(substr(x=locs, start = 3,3))
    sp.locs=sp.locs[!sp.locs==0]
    ml.locs=unique(substr(x=locs, start=6,6))
    ml.locs=ml.locs[!ml.locs==0]

    all.locs=list(ml.locs=ml.locs, sp.locs=sp.locs)

    ## Below function from answer provided by Joris Meys, on stackoverflow.com.
    ## Retrieved Dec 20, 2017 from https://stackoverflow.com/questions/7077710/sequence-length-encoding-using-r?rq=1
    seq.length <- function(x){
        if(!is.numeric(x)) x <- as.numeric(x)
        n <- length(x)
        y <- x[-1L] != x[-n] + 1L
        i <- c(which(y|is.na(y)),n)
        data.frame(
            start_index = as.vector(x[utils::head(c(0L,i)+1L,-1L)]),
            stop_index = as.vector(diff(c(0L,i)))
        )
    }

    ## End Function

    soil.DPs=c("DP1.00040.001", "DP1.00041.001", "DP1.00095.001")

    data.time.table=function(l, loc){
        l=as.character(l)
        if(loc=="sp"){
            l.data=Noble::sp.extract(data=data, sp = l)
        }else if(loc=="ml"){
            l.data=Noble::ml.extract(data=data, ml = l)
        }
        gaps=Noble::gap.find(data = l.data, time.agr = 30, return = "index")
        no.data.times=seq.length(gaps$no.data.indx)
        no.data.times$stop_index=(no.data.times$start_index+no.data.times$stop_index-1)
        location=paste0(toupper(loc),"-", l)
        no.data.times=data.frame(location=rep(location, times=nrow(no.data.times)),
                                 gap.start=l.data$startDateTime[no.data.times$start_index],
                                 gap.end=l.data$startDateTime[no.data.times$stop_index])
    }

    qf.time.table=function(l, loc){
        if(loc=="sp"){
            l.data=Noble::sp.extract(data=data, sp = l)
        }else if(loc=="ml"){
            l.data=Noble::ml.extract(data=data, ml = l)
        }
        gaps=Noble::gap.find(data = l.data, time.agr = 30, return = "index")
        no.qf.times=seq.length(gaps$no.qf.indx)
        no.qf.times$stop_index=(no.qf.times$start_index+no.qf.times$stop_index-1)
        location=paste0(toupper(loc),"-", l)
        no.qf.times=data.frame(location=rep(location, times=nrow(no.qf.times)),gap.start=l.data$startDateTime[no.qf.times$start_index], gap.end=l.data$startDateTime[no.qf.times$stop_index])
    }

    sp.no.data=lapply(all.locs$sp.locs, function(l) data.time.table(l, loc="sp"))
    sp.no.qf=lapply(all.locs$sp.locs, function(l) qf.time.table(l, loc="sp"))

    ml.no.data=lapply(all.locs$ml.locs, function(l) data.time.table(l, loc="ml"))
    ml.no.qf=lapply(all.locs$ml.locs, function(l) qf.time.table(l, loc="ml"))

    no.data.times=rbind(do.call(rbind, sp.no.data), do.call(rbind, ml.no.data))
    no.qf.times=rbind(do.call(rbind, sp.no.qf), do.call(rbind, ml.no.qf))

    no.data.times=no.data.times[!(no.data.times[,2]==no.data.times[,3]),] #weed out weird duplicates

    utils::write.csv(x = no.data.times, file = paste0(save.dir, "/", site, "_", bgn.month, "-", end.month, "_", short.name, "_NO_DATA.csv"),row.names = F)
    utils::write.csv(x = no.qf.times, file = paste0(save.dir, "/", site, "_", bgn.month, "-", end.month,  "_", short.name, "_NO_QF.csv"))

}
