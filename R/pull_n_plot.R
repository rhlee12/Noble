############################################################################################
#' @title  Returns PDF(s) of data for the specified site and data product

#' @author Cove Sturdevant

#' @description For a specified data product ID, a data frame of the availabilty of that product
#' for all NEON instrumented sites is returned. The output of data product availability is best
#' interpreted with the base \code{View()} function.
#'
#' @param sites.req The site, or character list of sites to return plots of.
#' @param bgn.month The start month to plot data for.
#' @param end.month The end month to plot data for.
#' @param dp.id Parameter of class character. The NEON data product code of the data product of interest.
#' @param save.dir The directory for data files and output PDFs to be saved to.
#' @param data.field Optional. The name of the measurement vaiable to plot. Defaults to the 'core' measurement for most products.
#' @param package Optional. The package type ("basic" or "expanded") to be downloaded.

#' @return Outputs a a PDF of plots data on of all measurement levesl, with one PDF per site.
#'

#' @keywords process quality, data quality, gaps, commissioning
#'
#' @examples
#' \dontrun{
#' # for a variable, "test.dir", holding a valid file path:
#' pull.n.plot(bgn.month = "2017-04",
#' end.month = "2017-05",
#' dp.id = "DP1.00001.001",
#' sites.req = "BLAN",
#' save.dir = getwd(),
#'  data.field = "windDirMean")
#' }


# changelog and author contributions / copyrights
#   Cove Sturdevant (2016-11-07)
#     original creation
#
#   Robert Lee (2017-07-17)
#     Updating function for Noble integration
#
##############################################################################################

pull.n.plot <- function(sites.req, bgn.month, end.month, dp.id, save.dir, data.field, package){
    options(stringsAsFactors = FALSE)
    time=NULL
    value=NULL
    qfFail=NULL

    #kpiList <- data.frame(read.csv("https://raw.githubusercontent.com/rhlee12/Data-Products/master/kpiList.csv", header = TRUE))
    time.agr=30


    test.qf = "finalQF"
    #ENTRY CONTROL
    #auto fill the data field, if not specified
    if(missing(data.field)){
        data.field = Noble::tis_pri_vars$data.field[Noble::tis_pri_vars$dp.id==dp.id]
    }

    if(interactive()&length(data.field)==0){
        data.field = readline(prompt = "No valid data field found, please enter one: ")
    }else if(!interactive()){stop("No valid data.field found, please enter one.")}

    if(missing(package)){
        package<-"basic"
    }

    pack.ctrl<-c("basic", "expanded")

    if(!package %in% pack.ctrl){
        package<-"basic"
        message("Invalid package type requested, defaulting to basic.")
    }

    DateBgn <- paste0(bgn.month, "-01")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")
    end_temp<- as.POSIXlt(paste0(end_temp, "-01"), tz="UTC")
    end_temp$mon<-end_temp$mon+1
    end_temp<-end_temp-lubridate::minutes(time.agr)-lubridate::seconds(1)
    DateEnd<-as.Date(end_temp)

    s<-1
    for (s in 1:length(sites.req)){

    domn<-Noble::tis_site_config$Domain[Noble::tis_site_config$SiteID==sites.req[s]]
        dataFile<- paste0("NEON.", domn,".", sites.req[s],".", dp.id, "_REQ_", DateBgn, "_", DateEnd, "_", time.agr,"min_", package, ".csv.gz")
        fullPath <- paste0(save.dir, "/", dataFile)
        if (!file.exists(fullPath)){
            print(paste("Currently downloading data for:", sites.req[s]))

            sink<-Noble::pull.data(site = sites.req[s], bgn.month = bgn.month, end.month = end.month,
                                   dp.id = dp.id, time.agr = time.agr, package = package, save.dir = save.dir)
        }
        rm(sink) # get rid of environment data

        if(is.null(data.field)){stop("No data.field specified or identifiable. Specify this parameter for this DP ID.")}

        # Read the requested data back in
        print(paste("Reading and plotting", sites.req[s], "data."))
        commData <- data.frame(utils::read.csv(fullPath, header = TRUE))

        if(dp.id=="DP1.00024.001"){
            commData=commData[,-which(grepl(pattern = "outPAR*", x = colnames(commData)))]
        }

        QFindex <- grep(pattern = "*finalQF\\.", colnames(commData), ignore.case = T)
        if(dp.id=="DP1.00001.001"){
            if(grepl(pattern = "*speed*", data.field, ignore.case = T)){
                QFindex = grep(pattern = "windSpeedFinalQF\\.", colnames(commData), ignore.case = T)
            }else if(grepl(pattern = "*dir*", data.field, ignore.case = T)){
                QFindex = grep(pattern = "windDirFinalQF\\.", colnames(commData), ignore.case = T)
                }
        }
        dataIndex <- grep(paste0(data.field), colnames(commData), ignore.case = T)
        timeStmp <- as.POSIXct(strptime(commData[,1], format="%Y-%m-%d %H:%M:%S", tz="UTC"))

        {grDevices::pdf(file=paste0(save.dir, "/", sites.req[s], "_", dp.id, "_", package, "_", data.field, ".pdf", sep=""), paper = "us")}

        niceColors<- c("0"="#41f299", "1"="#f25841", "NA"="black")

        for (idxPlot in 1:length(dataIndex)){

            nameData <- names(commData)[dataIndex[idxPlot]]
            data <- base::data.frame(time=timeStmp,data=commData[[dataIndex[idxPlot]]],qf=commData[[QFindex[idxPlot]]],
                                     nameData=nameData)

            nameQf=test.qf

            # Generate data completeness plot
            dataPlot <- base::data.frame(time=data$time,value=data$data,qfFail=data$qf)


            if(base::sum(data[[2]],na.rm=TRUE) == 0) {
                # No non-NA data, generate empty plot
                plotData <- ggplot2::ggplot(data=data,ggplot2::aes(x=time)) + ggplot2::geom_blank()
                grobData <- ggplot2::ggplotGrob(plotData)
            } else {
                # Data to plot!

                plotData <- ggplot2::ggplot(data=data,ggplot2::aes(x=time,y=data)) +
                    ggplot2::geom_line() +
                    ggplot2::geom_point(size=1) +
                    ggplot2::geom_point(data=dataPlot,ggplot2::aes(x=time, y=value, color=factor(qfFail))) +
                    ggplot2::scale_color_manual(values = niceColors, name = "Final Quality Flag", limits=c(0, 1, "NA"))+
                    #ggplot2::scale_colour_continuous(low = "#a50037", high = "#00a560")+
                    ggplot2::theme_bw() +
                    ggplot2::labs(x="Date/Time", y=nameData, title=sites.req[s])
                grobData <- ggplot2::ggplotGrob(plotData) # grab the grob for this plot for later manipulation
            }

            gridExtra::grid.arrange(grobData,nrow=1) # plot it

        } ##Plotting code
        {grDevices::graphics.off()}
        print(paste(sites.req[s], "complete."))
    }
}

