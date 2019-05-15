#Helper Functions
#..........................................................................................#
.air.var.plotting=function(test.data, save.dir){
    test.time = c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                  "03:30:00", "04:00:00")

    night.data=test.data[which(strftime(test.data$startDateTime, format="%H:%M:%S", tz=site.tz) %in% test.time),]
    night.data=data.frame(startDateTime=night.data$startDateTime, night.data[,grepl(pattern = "Variance", x = colnames(night.data))])
    m.night=reshape2::melt(night.data, id.vars="startDateTime")
    m.night$startDateTime=as.POSIXct(m.night$startDateTime, tz = site.tz)
    m.night$testPeriod=0
    m.night$testPeriod[frst.week[2]>m.night$startDateTime&m.night$startDateTime>=frst.week[1]]=1
    m.night$testPeriod[last.week[2]>m.night$startDateTime&m.night$startDateTime>=last.week[1]]=1

    var.plot=ggplot2::ggplot(data=m.night, ggplot2::aes(x=startDateTime, y=value, color=as.factor(variable)))+
        ggplot2::geom_rect(
            ggplot2::aes(xmin=as.POSIXct(frst.week[1], tz=site.tz),
                         xmax = as.POSIXct(frst.week[2], tz=site.tz),
                         ymin = -Inf,
                         ymax = Inf),
            fill = 'gray',
            alpha = 0.01)+
        ggplot2::geom_rect(
            ggplot2::aes(xmin=as.POSIXct(last.week[2], tz=site.tz),
                         xmax = as.POSIXct(last.week[1], tz=site.tz),
                         ymin = -Inf,
                         ymax = Inf),
            fill = 'gray',
            alpha = 0.01)+
        ggplot2::geom_point()+
        ggplot2::theme_light()+
        ggplot2::ggtitle(paste0(site, " overnight variance values for radiation data products"), subtitle = "Gray boxes indicate test periods")+
        ggplot2::xlab("Date")+
        ggplot2::geom_smooth()

    ggplot2::ggsave(plot = var.plot, device = "png", width = 8, height = 5, filename = paste0(site, "_night_vars.png"), path = .data.route(site = site, save.dir = save.dir), units = "in")

}

#..........................................................................................#
# Puts free-range data into a tidy file structure by site.
.data.org=function(dir){

    site.extract=function(file.name){
        site=stringr::str_extract(
            string = stringr::str_extract(
                string = file.name, pattern = "\\.[:upper:][:upper:][:upper:][:upper:]\\."
            ), pattern = "[:upper:][:upper:][:upper:][:upper:]"
        )
        return(site)
    }

    data.files=list.files(dir)[grepl(pattern = "\\.csv\\.gz", x = list.files(dir))]
    for(i in 1:length(data.files)){
        site=site.extract(data.files[i])
        domn=Noble::tis_site_config$domain[Noble::tis_site_config$site.id==site]
        if(!dir.exists(paste0(dir, domn, "-", site))){
            dir.create(paste0(dir, domn, "-", site))}
        file.copy(from = paste0(dir, data.files[i]), to = paste0(dir, domn, "-", site))
        file.remove(paste0(dir, data.files[i]))
    }
}

#Route data and results to appropriate files
.data.route=function(site, save.dir){
    domn=Noble::tis_site_config$domain[Noble::tis_site_config$site.id==site]
    data.route=paste0(save.dir, "/", domn, "-", site, "/")
    if(!dir.exists(data.route)){dir.create(data.route)}
    return(data.route)
}

.result.route=function(save.dir){
    result.dir=paste0(save.dir, "/Common/")
    if(!dir.exists(result.dir)){dir.create(result.dir)}
    result.route=paste0(result.dir, "results.csv")
    return(result.route)
}

#..........................................................................................#
#generate call.df####
.gen.call.df=function(bgn.month, end.month, site=site, dp.id=dp.id, time.agr=time.agr, package=package){

    bgn_temp <- as.Date(paste0(bgn.month, "-01"), tz="UTC")
    end_temp <- as.Date(paste0(end.month, "-01"), tz="UTC")

    date_range<-substr(seq.Date(bgn_temp, end_temp, "month"), 0, 7)

    site_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/sites/", site), unexpected.escape = "skip")$data

    prod_meta=rjson::fromJSON(file = paste0("http://data.neonscience.org/api/v0/products/", dp.id), unexpected.escape = "skip")$data

    prod_indx=grep(site_meta$dataProducts, pattern = dp.id)
    site_indx=grep(prod_meta$siteCodes, pattern = site)

    if(length(prod_indx)==0){
        stop(paste0(dp.id, " is not currently available at ", site, " via the API."))
    }

    site_options=data.frame(avail_months=unlist(site_meta$dataProducts[[prod_indx]]$availableMonths), urls= unlist(site_meta$dataProducts[[prod_indx]]$availableDataUrls))

    # Stop if no data
    if(length(site_options$avail_months)==0){stop(paste0(dp.id, " is missing at ", site))}

    all_data_urls <- unlist(unique(site_options$urls))

    #construct temporary API call urls
    url_index<-lapply(date_range, function(x) grep(pattern=x, all_data_urls))
    temp_data_urls<-all_data_urls[unlist(url_index)]

    if(length(temp_data_urls)==0){
        stop("Data were missing in specified date range at ", site, ". Check ", dp.id, " avalability with neon.avail")
        }

    #For found DPs, given the Kpi, pull hosted metadata via API

    api_data<-lapply(as.list(temp_data_urls), function(x) as.list(rjson::fromJSON(file = as.character(x), unexpected.escape = "skip")))

    # build a list of URLs served by the API
    url_list<-c()
    i<-1
    for(i in 1:length(api_data)){
        tempList<-api_data[[i]]$data$files
        listLeng<-length(tempList)
        if(listLeng==0){break()}
        for(j in 1:listLeng){
            url_list<-append(url_list, tempList[[j]]$url)
        }
    }

    # Weed out XML links
    url_list=url_list[!(grepl(pattern = "xml", x= url_list))]

    #Try to handle name excpetions
    exceptions=c("DP1.00005.001", "DP1.00041.001")

    if((dp.id %in% exceptions)){ #Why, oh why does bio temp have to be different on the API
        url_list<-url_list[grepl(pattern = paste0(time.agr, "_min*"), x= url_list)]
    }else{
        url_list=url_list[grepl(pattern = paste0(time.agr,"min*"), x= url_list)|grepl(pattern = paste0(time.agr, "_min*"), x= url_list)] #should catch unknown exceptions
    }

    #Looking for location info
    loc_list_temp=stringr::str_extract(string=url_list, pattern = paste0(dp.id, "\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d"))
    loc_list=stringr::str_sub(loc_list_temp, start = 15, end = 21)
    if(all(is.na(loc_list_temp))){
        loc_list_temp=stringr::str_extract(string=url_list, pattern ="\\.\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d")
        loc_list=stringr::str_sub(loc_list_temp, start = 1, end = 8)
    }

    dp_list<-rep(dp.id, times=length(loc_list))

    call.df<-as.data.frame(cbind(url_list, dp_list, loc_list))

    # Order the call.df by data product, then location
    call.df<-call.df[order(call.df$dp_list, call.df$url_list),]
    call.df=call.df[which(grepl(x=call.df$url_list, pattern=package)),] #Keep only our package type
    call.df=call.df[which(grepl(x=call.df$url_list, pattern="\\.csv")),] #Keep only CSVs
    call.df=call.df[which(!grepl(x=call.df$url_list, pattern="variables")),] #weed out varaible tables

    return(call.df)
}
#..........................................................................................#
# Private function for returning the domain of a site
.get.domain=function(site){
    neon.sites=Noble::is_site_config$SiteID
    if(!site %in% neon.sites){stop(paste0(site, "is not a valid NEON site. \nPlease input a 4-letter NEON site code/"))}
    domain=Noble::is_site_config$Domain[Noble::is_site_config$SiteID==site]
}

#..........................................................................................#
# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
.percent.buffer=function(site, bgn.month, end.month, save.dir){

    buff_low=NULL
    buff_hi=NULL
    ## Read in the site info and threshold info:
    if(missing(save.dir)){save.dir=tempdir()}

    #get our data
    example.data=Noble::pull.data(site=site,
                                  dp.id = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    example.data=.un.ml.ize(example.data)

    # ATBD CALCS
    # Section 5.1
    ## Distorted Flow Test

    ###General parameters

    DF.info=Noble::distorted.field(site=site)
    message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])

    if(DF.info$distortedField[1]>DF.info$distortedField[2]){
        DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$buff_low[1], upper=360))])
        DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
        DF=rbind(DF1, DF2)
    }else{
        DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
    }
    LB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_low[1], upper=buff_low[2]))]
    UB_dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower = buff_hi[1], upper=buff_hi[2]))]

    pcnt.in.buffer=(length(UB_dir)+length(LB_dir))/length(example.data$windDirMean)*100
    message(paste0("Finshed with ", site))
    return(pcnt.in.buffer)
}

#..........................................................................................####
# Private function for calculating the percent of wind measurements falling due to wind coming
# through the buffers on either side of the distorted flow field
.percent.distorted=function(site, bgn.month, end.month, save.dir){

    if(missing(save.dir)){save.dir=tempdir()}

    #get our data
    example.data=Noble::pull.data(site=site,
                                  dp.id = "DP1.00001.001",
                                  bgn.month = bgn.month,
                                  end.month = end.month,
                                  time.agr = 30,
                                  package = 'expanded',
                                  save.dir = save.dir)

    example.data=.un.ml.ize(example.data)

    DF.info=distorted.field(site=site)
    message("Site: ", site, ". DFF low: ", DF.info$distortedField[1], ", DFF high: ", DF.info$distortedField[2])
    if(DF.info$distortedField[1]>DF.info$distortedField[2]){
        DF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=360))])
        DF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DF.info$distortedField[2]))])
        DF=rbind(DF1, DF2)
    }else{
        DF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DF.info$distortedField[1], upper=DF.info$distortedField[2]))])
    }
    #UB_dir=example.data$windDirMean.000.010[which(data.table::between(example.data$windDirMean.000.010, lower = buff_hi[1], upper=buff_hi[2]))]

    DFF_plus_B=c(lower=(DF.info$distortedField[1]-10), upper=(DF.info$distortedField[2])+10)

    if(DFF_plus_B[1]>DFF_plus_B[2]){
        BDF1=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =(DFF_plus_B[1]), upper=360))])
        BDF2=data.frame(dir=example.data$windDirMean[which(data.table::between(x = example.data$windDirMean, lower =0, upper=DFF_plus_B[2]))])
        BDF=rbind(DF1, DF2)
    }else{
        BDF=data.frame(dir=example.data$windDirMean[which(data.table::between(example.data$windDirMean, lower =DFF_plus_B[1], upper=DFF_plus_B[2]))])
    }
    All=(length(BDF[,1]))/length(example.data$windDirMean)*100
    DF.field.only=(length(DF[,1]))/length(example.data$windDirMean)*100
    out=c(All, DF.field.only)
    return(out)
}


#..........................................................................................##
# Returns PNGs of data for the specified site and data product
.pull.n.plot.png <- function(sites.req, bgn.month, end.month, dp.id, save.dir, data.field, package){

    qfFail=NULL
    time=NULL
    value=NULL

    time.agr=30
    test.qf = "finalQF"

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

        domn<-Noble::tis_site_config$domain[Noble::tis_site_config$site.id==sites.req[s]]
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

        #{pdf(file=paste0(save.dir, "/", sites.req[s], "_", dp.id, "_", package, "_", data.field, ".pdf", sep=""), paper = "us")}

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
                #grobData <- ggplot2::ggplotGrob(plotData)
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
                # grobData <- ggplot2::ggplotGrob(plotData) # grab the grob for this plot for later manipulation
            }
            ggplot2::ggsave(filename =paste0(sites.req[s], "_", dp.id, "_", package, "_", nameData, ".png"), plot = plotData, device = "png", path = save.dir, width = 6, height = 3, units = "in")
            #gridExtra::grid.arrange(grobData,nrow=1) # plot it

        } ##Plotting code
        {grDevices::graphics.off()}
        print(paste(sites.req[s], "complete."))
    }
}

#..........................................................................................####
# Saves a plot of flow metrics, and returns the percent of records with good flow
# (percent of flow metrics where flowPassQM=100)

.flow.effect=function(site, bgn.month, end.month, save.dir){
    Date=NULL
    value=NULL
    variable=NULL
    domn=Noble::tis_site_config$domain[Noble::tis_site_config$site.id==site]
    data.dir=.data.route(site=site, save.dir = save.dir)
    data=Noble::pull.data(site=site,
                          dp.id = "DP1.00002.001",
                          bgn.month = bgn.month,
                          end.month = end.month,
                          package = "expanded",
                          time.agr = 30,
                          save.dir = data.dir)

    flow.flags=data.frame(Date=data[,1], data[grepl(pattern = "flow", x = colnames(data), ignore.case = T)])
    #colnames(flow.flags)=gsub(pattern = "\\.||\\d", replacement = "", x = colnames(flow.flags))

    melt.flow=reshape2::melt(flow.flags, id.vars="Date", na.rm=T)
    melt.flow$variable=gsub(pattern = "\\.||\\d", replacement = "", x = melt.flow$variable)
    melt.flow$Date=as.POSIXct(melt.flow$Date)


    niceColors<- c("flowFailQM"="#f25841", "flowNAQM"="grey", "flowPassQM"="#41f299")

    # melt.flow=reshape2::melt(data=flags.only[grepl(pattern = "flow", x = colnames(flags.only), ignore.case = T)]

    #flags=ggplot(melt.flags, aes(x = Date, y = value, fill=factor(variable)))+geom_bar(stat = 'identity')

    plot=ggplot2::ggplot(melt.flow, ggplot2::aes(x=Date, y = value/3, fill=factor(variable)))+
        ggplot2::theme_bw()+
        ggplot2::geom_bar(stat = 'identity', width = 10000)+
        ggplot2::scale_y_continuous(limits = c(0,100))+
        ggplot2::theme(axis.text.x = ggplot2::element_blank())+
        ggplot2::scale_fill_manual(name = "Flow Quality Metrics", values=niceColors)+
        ggplot2::labs(x=paste0(zoo::as.yearmon(bgn.month, format="%Y-%m"), " to ", zoo::as.yearmon(end.month, format="%Y-%m")),
                      y="Flagging distribution (% of measuremnts)",
                      title=paste0(domn, "-", site))

    ggplot2::ggsave(filename = paste0(site, "_", bgn.month, "-", end.month, "_flow.png"),
                    path = paste0(save.dir, "/"),
                    plot = plot,
                    device = 'png',
                    width = 5,
                    height = 3.5,
                    units = "in",
                    dpi=300)

    #math for percent of file that passes.
    pass=flow.flags[,grepl(x=colnames(flow.flags), pattern = "Pass")]
    sum.pass=sum(pass, na.rm=T)
    total=(length(flow.flags)/(Noble::tis_site_config$Num.of.MLs[Noble::tis_site_config$site.id==site]-1))*length(flow.flags[,1])
    pcnt.flow.pass=round(sum.pass/total, digits = 2)
    return(pcnt.flow.pass)

}

##..........................................................................................#
# Make a sequence of months between bgn.month and end.month
.month.seq=function(bgn.month, end.month){
    time1=zoo::as.Date.yearmon(zoo::as.yearmon(bgn.month))
    time2=zoo::as.Date.yearmon(zoo::as.yearmon(end.month))
    temp.seq=seq.Date(from=time1, to = time2, by="1 month")

    out=substr(temp.seq, start = 1, stop=7)
    return(out)
}

#..........................................................................................##
# Create wind roses for NEON instrumented sites binned by quality flag
.plot.qf.wind.rose = function(site, bgn.month, end.month, ml, speed.bins, dir.bins){

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
    data<-Noble::pull.data(site = site, dp.id = "DP1.00001.001", bgn.month = bgn.month, end.month = end.month, time.agr = 30, package="basic", save.dir = tempdir())

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
        dir.qf.index = as.numeric(grep(x=colnames(data.by.ml), pattern = "windDirFinalQF\\.", ignore.case = T))
        speed.indx <- as.numeric(grep(x=colnames(data.by.ml), pattern = "windSpeedMean", ignore.case = T))

        if(length(speed.indx)==0){stop("No wind data found!")}


        direct<-data.by.ml[,dir.indx]
        speed<-data.by.ml[,speed.indx]
        qf=paste0("QF=",data.by.ml[,dir.qf.index])


        temp.df<-as.data.frame(cbind("Dir"=as.numeric(direct), "Speed"=as.numeric(speed), "QualityFlag"=qf, "ML"=rep(mls[n], length(direct))))
        temp.df<-temp.df[temp.df$Dir>=0,]
        all<-rbind(all, temp.df)
    }
    all<-all[!is.null(all)]

    # Set up output parameters for plot
    degreeSteps<-as.numeric(360/dir.bins)
    dir.bin.seq<-seq(0, 360, by=360/dir.bins)
    all.binned<-cbind(all, SpeedCut= cut(as.numeric(all$Speed), breaks = speed.bins), DirCut= cut(as.numeric(all$Dir), breaks = dir.bin.seq))
    all.binned<-stats::na.omit(all.binned)

    # Make labels and title
    bgnLabels<- unique((dir.bin.seq-(degreeSteps/2))%%360)
    endLabels<- unique((dir.bin.seq+(degreeSteps/2))%%360)
    dirLabels<-base::paste0(bgnLabels, "-", endLabels)
    if(ml.case){titleString =base::paste0(site, " wind data from ", bgn.month, " through ", end.month)}else{
        titleString<-base::paste0("ML", ml, "-", site, " wind data from ", bgn.month, " through ", end.month)
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
        ggplot2::scale_fill_discrete(h = c(0, 240), l=65, c=100, name="Wind Speed, m/s")+
        ggplot2::facet_wrap(~QualityFlag)

    # If we didn't get ml specified, make a faceted plot
    if(ml.case){
        plot<-plot+ggplot2::facet_grid(QualityFlag~ML)
    }
    return(plot)
}

#..........................................................................................##
# Print human-readable location names
.translate.hor.ver=function(h.v){
    h.v=stringr::str_extract(h.v, pattern = "[0-9]{3}\\.[0-9]{3}")

    split.hv=unlist(strsplit(h.v, split = "\\."), recursive = F)
    hor=unlist(strsplit(x = split.hv[1], split = "|"))
    ver=unlist(strsplit(x = split.hv[2], split = "|"))
    location="NULL"
    if(all(hor=="0")){
        if(all(ver[c(1,3)]=="0")){
            location=paste0("Measurement Level ", ver[2])
        }else if(ver[2]!="0"&ver[3]!="0"){
            location=paste0("Measurement Level ", ver[2], ".", ver[3])
        }
    }else if(all(hor[1:2]=="0")&hor[3]!="0"){
        if(all(ver=="0")){
            location=paste0("Soil Plot ", hor[3])
        }else if(ver[1]=="5"){
            location=paste0("Soil Plot ", hor[3], ", depth ", ver[3])
        }
    }else if(hor[1]!="0"&all(ver=="0")){
        if(hor[1]=="2"){
            location="On-shore MET station"
        }else if(hor[1]=="9"){
            location="DFIR"
        }else if(h.v=="220.000"){
            location="Secondary tipping bucket"
        }else if(hor[1]=="3"&all(ver=="0")){
            location=paste0("Ground water well ", hor[3])
        }
    }
    return(location)
}

#..........................................................................................##
# Downloads and performs process quality checks on NEON data, given specifc dates
.tis.pq.test<-function(site = "CPER", dp.id = "DP1.00001.001", prin.vars,  bgn.date = "2017-05-15", end.date = "2017-06-15", time.agr = 30, package="basic", save.dir, q.th=95, v.th=90){

    bgn.month=substr(bgn.date, 0, 7)
    end.month=substr(end.date, 0, 7)

    quant_threshold=q.th
    valid_threshold=v.th

    if(missing(q.th)){
        q.th=95
        quant_threshold=q.th
    }
    if(missing(v.th)){
        v.th=90
        valid_threshold=v.th
    }

    #Make domain-specific directory
    domn=Noble::tis_site_config$domain[which(Noble::tis_site_config$site.id==site)]
    site.dir=paste0(save.dir, "/", domn, "-", site, "/")

    if(!dir.exists(site.dir)){
        dir.create(site.dir)
    }

    #pull data
    test.data=Noble::pull.data(site = site, dp.id = dp.id, bgn.month = bgn.month, end.month = end.month, time.agr = time.agr, package=package, save.dir=site.dir)
    test.data=Noble::date.extract(data=test.data, bgn.date = bgn.date, end.date = end.date)

    for(i in 1:length(prin.vars)){
        data.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i], "Mean*"))

        qf.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i], "FinalQF*"))
        qf.indx<-append(qf.indx, grep(x=colnames(test.data), pattern="^finalQF*"))

        if(prin.vars[i]=="inSW"){
            data.indx=data.indx[-which(grepl(x=colnames(test.data[,data.indx]), pattern = "003.000"))]
            qf.indx=qf.indx[-which(grepl(x=colnames(test.data[,qf.indx]), pattern = "003.000"))]
        }
        #special case for precip
        if(prin.vars[i]=="priPrecipBulk"){
            data.indx<-grep(x=colnames(test.data), pattern=paste0("^", prin.vars[i]))
            qf.indx<-grep(x=colnames(test.data), pattern=("priPrecipFinalQF\\."), ignore.case = T)
        }


        bgn.day=as.Date(paste0(bgn.month, "-01"))
        end.day=Noble::last.day.time(end.month = end.month, time.agr = 1440)
        end.day=as.POSIXct(end.day)


        days=round(difftime(end.date, bgn.date, units="days"), digits = 2)
        end.day=lubridate::round_date(end.day, "day")

        pq.data<-test.data[,data.indx]
        qf.data<-test.data[,qf.indx]

        num.nas<-sum(is.na(pq.data))
        num.data<-sum(!is.na(pq.data))

        data.quant<-round(100*(num.data/(num.nas+num.data)), digits = 2)

        num.qf.fail<-sum(qf.data==1, na.rm=TRUE)
        num.qf.pass<-sum(qf.data==0, na.rm = TRUE)
        num.qf.na<-sum(is.na(qf.data))

        data.valid<-round(100*(num.qf.pass/(num.qf.pass+num.qf.fail+num.qf.na)), digits = 2)


        #Set passing thresholds, based on var tested. Add to this area as functions or conditions are added
        ## direct radiation has an ATBD implementation error- revert to full thresholds.
        # if(prin.vars[i]=="dirRad"){
        #     #direct and diffuse caluculated values
        #     quant_threshold=Noble::dirRad.threshold(site = site, bgn.month = bgn.month, end.month = end.month, excuse = 5)
        #     valid_threshold=Noble::dirRad.threshold(site = site, bgn.month = bgn.month, end.month = end.month, excuse = 10)
        #}
        if(prin.vars[i]=="SHF"){
            #Soil heat flux specific values
            quant_threshold=95
            valid_threshold=(90-15.38)
        }
        if(prin.vars[i]=="soilTemp"){
            quant_threshold=94.6
            valid_threshold=89.87
        }


        dq.rslt<-data.frame(site=site,
                            time_performed=as.character(Sys.time()),
                            begin_month=bgn.date,
                            end_month=end.date,
                            days_tested=days,
                            data_product= dp.id,
                            variable_tested=prin.vars[i],
                            data_quantity=data.quant,
                            data_validity=data.valid,
                            quant_threshold= quant_threshold,
                            valid_threshold=valid_threshold
        )

        rslt.dir=paste0(save.dir, "/", "Common/")
        if(!dir.exists(rslt.dir)){
            dir.create(rslt.dir)
        }

        if(file.exists(paste(rslt.dir,"results.csv",sep = "/"))){
            dq.rpt <- data.frame(utils::read.csv(file = paste(rslt.dir,"results.csv",sep = "/"), header = T, stringsAsFactors = T))
            dq.rpt <- rbind(dq.rpt, dq.rslt)
            utils::write.csv(x = dq.rpt, file = paste(rslt.dir,"results.csv",sep = "/"), row.names = F)
        }
        else{
            utils::write.csv(x = dq.rslt, file = paste(rslt.dir,"results.csv",sep = "/"), col.names = T, row.names = F)
        }
    }
}

#..........................................................................................#
#Average individual tower-based measurements accross all MLs
.un.ml.ize=function(data, keep.na){
    if(missing(keep.na)){keep.na=T}
    if(!is.logical(keep.na)){message("keep.na is not logical, defaulting to TRUE.")
        keep.na=T}
    data=data[,-2]
    data.melt=reshape2::melt(data, id.vars="startDateTime")
    data.melt$variable=stringr::str_replace(string = data.melt$variable, pattern = "\\.\\d\\d\\d\\.\\d\\d\\d", replacement = "")
    if(keep.na==F){data.melt=data.melt[-is.na(data.melt$value),]}

    data.out=reshape2::dcast(data = data.melt, formula = startDateTime~variable, fun.aggregate = mean)
    return(data.out)
}

#..........................................................................................####
# Variance drift testing
.var.drift.test=function(single.var.data, raw.dir, site, plot=FALSE){

    # periods=.var.periods(bgn.month = bgn.month, end.month = end.month)
    # frst.per=periods$frst.per
    # last.per=periods$last.per
    #Put into massive data frame
    # if(class(raw.var.data)=="list"){
    # var.data=do.call(cbind, raw.var.data)}else{var.data=raw.var.data}

    #var.data=data.frame(startDateTime=var.data[,1], var.data[,grepl(pattern = "variance", x = colnames(var.data), ignore.case = T)])
    #var.data=var.data[,-which(grepl(pattern = "*LW*", x = colnames(var.data)))]

    # Convert to local time
    time.zone=Noble::tis_site_config$time.zone[Noble::tis_site_config$site.id==site]
    #browser()
    var.data=single.var.data

    var.data$startDateTime=as.POSIXct(var.data$startDateTime, tz="UTC")
    var.data$startDateTime=as.POSIXct(format(var.data$startDateTime, tz=time.zone, usetz = T), tz=time.zone, usetz = T)

    test.time = c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:30:00", "03:00:00",
                  "03:30:00", "04:00:00")

    night.vars=var.data[which(strftime(var.data$startDateTime, format="%H:%M:%S", tz=time.zone) %in% test.time),]

    y=night.vars[,2]
    x=seq_along(night.vars$startDateTime)

    if(!all(is.na(y))){
        out=stats::lm(y~x)
        slope=out$coefficients[2]
        plot(night.vars)
    }else{
        slope=NA
    }
    out=c(stream=colnames(night.vars)[2], slope=slope)
    return(out)
}

#..........................................................................................####
#Define two time periods for variance testing - Used with old Levene test of ratio of variances
.var.periods=function(bgn.month, end.month){
    if(bgn.month==end.month){
        test.days=lubridate::days_in_month(zoo::as.yearmon(bgn.month))
        half=as.numeric(unlist(strsplit(x=as.character(test.days/2), split = "\\."))[1])
    }else{
        test.days=abs(difftime(time1 = as.Date(paste0(bgn.month, "-01")), time2 = as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)), units = "days"))
        half=as.numeric(unlist(strsplit(x=as.character(test.days/2), split = "\\."))[1])
    }
    out=list(
        frst.per=c(as.Date(paste0(bgn.month, "-01")), as.Date(paste0(bgn.month, "-01"))+half),
        last.per=c(as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1))-half, as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)))
    )
    return(out)
}

#..........................................................................................####
# Summarize the QMs contributing to flagging of 2D wind DIRECTION data
.wind.qm.summary=function(site, bgn.month, end.month, save.dir){
    if(missing(save.dir)){save.dir=tempdir()}
    data<-Noble::pull.data(site = site,
                           dp.id = "DP1.00001.001",
                           bgn.month = bgn.month,
                           end.month = end.month,
                           time.agr = 30,
                           package="expanded",
                           save.dir = save.dir)

    dir.QM.only=data[,(grepl(colnames(data), pattern = "windDir") & grepl(x=colnames(data), pattern = "QM"))]

    qm.sums=data.frame(startDateTime=NA,rbind(colSums(dir.QM.only>20, na.rm=T)))
    qm.count.avgs=.un.ml.ize(qm.sums)
    qm.count.percent=round(qm.count.avgs/length(data[,1])*100, digits = 2)
    qm.count.percent=qm.count.percent[,-1]
    qm.count.percent=data.frame(Site=site, qm.count.percent)
    return(qm.count.percent)
}

#..........................................................................................####
# 2D wind validity (for PQ testing) that ommits Dist. Flow
.wind.validity=function(data.quant, data){
    wind.qms=colnames(data)[grep(pattern = "QM", ignore.case = F, x = colnames(data))]
    # wind.qms=unique(gsub(x = wind.qms, pattern = ".[0-9]{3}.[0-9]{3}", replacement = ""))
    qm.exclude=c("windDirDistortedFlowFailQM", "Pass", "Alpha")
    for(i in 1:length(qm.exclude)){
        wind.qms=wind.qms[!grepl(pattern = qm.exclude[i], x = wind.qms, ignore.case = F)]
    }
    wind.qms=as.character(na.omit(wind.qms))

    qf.subset=data[,wind.qms]
    null.cols=which(colSums(is.na(qf.subset))==nrow(qf.subset))
    qf.subset=qf.subset[,-null.cols]
    passes=apply(X=qf.subset, MARGIN = 1, function(row) !any(row>20, na.rm = F))
    out=round(length(which(passes))/nrow(data)*100, digits=2)
    return(out)
}

##..........................................................................................#
#writes results files out
.write.results=function(result, save.dir){
    if(file.exists(.result.route(save.dir))){
        temp.result = data.frame(utils::read.csv(file = .result.route(save.dir), header = T, stringsAsFactors = F))
        dq.rpt = rbind(temp.result, result)
        utils::write.csv(x = dq.rpt, file = .result.route(save.dir), row.names = F)
    }
    else{
        utils::write.csv(x = result, file = .result.route(save.dir), col.names = T, row.names = F)
    }
}

#### OLD CODE PRESERVED IN CASE ####
#~~~~~~~~~~~~~~~~~~ VARAIANCE TESTING FOR AAT ####
# #Make a sequence of dates and times for the requested period
# bgn_temp = base::as.Date(base::paste0(bgn.month, "-01"), tz="UTC")
# end_temp = base::as.Date(base::paste0(end.month, "-01"), tz="UTC")
# bgn_temp = base::as.POSIXct(base::paste0(bgn.month, "-01"), tz="UTC")
# end_temp = base::as.POSIXlt(base::paste0(end_temp, "-01"), tz="UTC")
# end_temp$mon=end_temp$mon+1
# #end_temp=end_temp-lubridate::minutes(30)-lubridate::seconds(1)
#
# frst.week=c(as.Date(paste0(bgn.month, "-01")), as.Date(paste0(bgn.month, "-01"))+7)
# last.week=c(as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1))-7, as.Date(Noble::last.day.time(end.month = end.month, time.agr = 1)))
#
#
# group.one=Noble::date.extract(data = test.data, bgn.date = bgn_temp, end.date =bgn_temp+lubridate::days(15))
# group.one=base::data.frame(startDateTime=group.one$startDateTime, group.one[,grepl(pattern = "tempSingleVariance", x = colnames(group.one))|grepl(pattern = "tempTripleVariance", x = colnames(group.one))])
# group.one=group.one[lubridate::hour(group.one$startDateTime) %in% c(0:5),]
# group.two=Noble::date.extract(data = test.data, bgn.date = end_temp-lubridate::days(15), end.date = end_temp-lubridate::minutes(30)-lubridate::seconds(1))
# group.two=base::data.frame(startDateTime=group.two$startDateTime, group.two[,grepl(pattern = "tempSingleVariance", x = colnames(group.two))|grepl(pattern = "tempTripleVariance", x = colnames(group.two))])
# group.two=group.two[lubridate::hour(group.two$startDateTime) %in% c(0:5),]
#
# f.test=c()
# for(i in 2:length(colnames(group.one))){
#     f.test=append(f.test, try(stats::var.test(x=as.numeric(group.one[,i]), y = as.numeric(group.two[,i]), ratio = 1)$p.value, silent = T))
# }
# f.test[unlist(lapply(f.test, function(f) grepl(pattern = "not enough", x = f)))]=NA
# #if(class(f.test)=="try-error"){f.test=NA}
# f.test=as.numeric(f.test)
# mean=mean(stats::na.omit(f.test), na.rm = T)
# f.test=append(f.test, c("mean"=mean))
# variance=data.frame(ML=c(seq(Noble::tis_site_config$num.of.mls[Noble::tis_site_config$site.id==site]), "Mean"), P_value=f.test)
#EOF####



