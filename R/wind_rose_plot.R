############################################################################################
#' @title  Create wind roses for NEON instrumented sites

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a spceified site and time range, produce a wind rose plot. If the "ml" (measurement level)
#' parameter is specified, a ggplot2 object for that measurement level is produced. Otherwise, a ggplot2 object of a
#' faceted plot of all available measurement levels is returned.
#'
#' @param site NEON site to produce the wind rose plot.
#' @param bgn.month The start month for wind data to plot.
#' @param end.month The end month for wind data to plot.
#' @param ml Optional. Used to specifiy what measurement level should be plotted.
#' @param speed.bins Optional. The number of bins for wind speed to be plotted in.
#' @param dir.bins Optional. The number of bins for wind directions to be plotted in.
#'
#' @return Outputs a ggplot2 object of the generated wind roses

#' @keywords process quality, data quality, gaps, commissioning

#' @examples
#' \dontrun{
#' CPER<-plot.wind.rose(site="CPER",
#' bgn.month="2017-01",
#' end.month="2017-02",
#' ml=2,
#' speed.bins=10,
#' dir.bins=36)
#' }

#' @export wind.rose.plot

# changelog and author contributions / copyrights
#   Robert Lee (2017-07-10)
#     original creation
#
##############################################################################################

wind.rose.plot = function(site, bgn.month, end.month, ml, speed.bins, dir.bins){
    options(stringsAsFactors = F)
    DirCut=NULL
    SpeedCut=NULL

    time.agr = 30
    ml.case=missing(ml)

    # Massage dates for checking
    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)

    # Return site metadata and products
    #site_meta<-nneo::nneo_site(site)

    # Warn about being too ambitious with this
    if(as.numeric(difftime(end_temp, bgn_temp))>=92){message("More than 3 months of data requested, may take a long time...")}

    # Return data
    data<-pull.data(site = site, dp.id = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package="basic", save.dir = tempdir())

    # Set default bin breakdowns
    if(missing(speed.bins)){speed.bins=10}
    if(missing(dir.bins)){dir.bins=36}

    # Break out what MLs were returned
    temp<-(strsplit(colnames(data[,3:length(colnames(data))]), split = "\\."))
    temp=do.call(rbind, temp)
    #temp<-data.frame(temp[-1], stringsAsFactors=F)
   #temp<-unlist((temp[3,]))
    mls<-unique(temp[,3])
    clean.mls<-gsub(x=mls, pattern = "0", replacement = "")

    # If ML was specified, make sure it was in the found MLs, and then make sure only it is written
    if(!missing(ml)){
        if(!paste0("0", ml, "0") %in% mls){
            message("Specified ML not avaiable at this site. Pick from:")
            stop(paste(unlist(clean.mls)))
        }
        mls<-ml
    }

    # Build a DF of relevant data and MLS associated
    all<-NULL
    for(n in 1:length(mls)){
        data.by.ml<- data[,which(grepl(colnames(data), pattern=mls[n]))]

        dir.indx<- as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirMean", ignore.case = T))
        speed.indx <- as.numeric(grep(x=colnames(data.by.ml), pattern = "windSpeedMean", ignore.case = T))

        if(length(speed.indx)==0){stop("No wind data found!")}


        direct<-data.by.ml[,dir.indx]
        speed<-data.by.ml[,speed.indx]


        temp.df<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed), "ML"=rep(mls[n], length(direct))))
        temp.df<-temp.df[temp.df$Dir>=0,]
        all<-rbind(all, temp.df)
    }
    all<-all[!is.null(all)]

    # Set up output parameters for plot
    degreeSteps<-as.numeric(360/dir.bins)
    dir.bin.seq<-seq(0, 360, by=360/dir.bins)
    all.binned<-cbind(all, SpeedCut = cut(as.numeric(all$Speed), breaks = speed.bins), DirCut= cut(as.numeric(all$Dir), breaks = dir.bin.seq))
    all.binned<-stats::na.omit(all.binned)

    # Make labels and title
    bgnLabels<- unique((dir.bin.seq-(degreeSteps/2))%%360)
    endLabels<- unique((dir.bin.seq+(degreeSteps/2))%%360)
    dirLabels<-paste0(bgnLabels, "-", endLabels)
    if(ml.case){titleString =paste0(site, " wind data from ", bgn.month, " through ", end.month)}else{
    titleString<-paste0("ML", ml, "-", site, " wind data from ", bgn.month, " through ", end.month)
}
    #Make and prettify the plot
    plot<-ggplot2::ggplot(data = all.binned, ggplot2::aes(x=DirCut, fill=SpeedCut, colors=factor(SpeedCut)))+
        ggplot2::geom_bar(width = .95, show.legend = T)+
        ggplot2::theme_linedraw()+
        ggplot2::coord_polar(theta = "x", start = 0)+
        ggplot2::xlab("")+
        ggplot2::ylab("Count")+
        ggplot2::labs(title=titleString)+
        ggplot2::scale_x_discrete(labels=endLabels)+
        ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name="Wind Speed, m/s")

    # If we didn't get ml specified, make a faceted plot
    if(ml.case){
        plot<-plot+ggplot2::facet_wrap(~ML)
    }
    return(plot)
}
