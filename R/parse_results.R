############################################################################################
#' @title  Parse a Raw Commissioning Results File for Most Recent Results

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Given a directory in the SCA, this function output only the most recent test results
#' from the 'results.csv' file in the
#'
#' @param test.dir The top level of a given test directory in the SCA (not the "Common" folder).
#' @param write.summary Logical, refering to if a simplified test summary should be written in
#' the Common folder of the specified directory. If TRUE, a summary_results.csv will be generated.
#' @param target_bgn Optional- if specified the parsed results will be restricted to the date range defined by target_bgn and target_end. Both must be entered, and in the format "YYYY-MM".
#' @param target_end Optional- if specified the parsed results will be restricted to the date range defined by target_bgn and target_end. Both must be entered, and in the format "YYYY-MM".
#'
#' @return The most recent test results from the 'results.csv' file in the
#' @export
#' @keywords process quality, data quality, gaps, commissioning
# changelog and author contributions / copyrights
#   Robert Lee (2018-01-15)
#     original creation
#   Robert Lee (2019-03-06)
#     Adding target date vars
#
##############################################################################################

parse.results=function(test.dir, write.summary=T, target_bgn, target_end){
    options(stringsAsFactors=FALSE)
    if(!missing(target_bgn)&!missing(target_end)){target_dates=TRUE}else{target_dates=FALSE}
    if(!is.logical(write.summary)){write.summary=TRUE}
    results = data.frame(utils::read.csv(.result.route(save.dir = test.dir), stringsAsFactors = FALSE))
    using.locs=FALSE
    if(any(grepl(pattern = "sensor_location", colnames(results)))){
        using.locs=TRUE
        results=data.frame(utils::read.csv(.result.route(save.dir = test.dir), stringsAsFactors = FALSE, colClasses = c("sensor_location"="character")))
    }

    # Important! Only reads the most recent results data per site into the RMD #
    siteList = (unique(results$site))
    numbSites = as.numeric(length(siteList))


    results=results[with(results, order(site, time_performed, data_product)),]

    parsed.results=data.frame()

    for (k in 1:numbSites) {
        siteIndex=grep(pattern = siteList[k], results$site)
        siteOnly =results[siteIndex,]
        if(target_dates){
            siteOnly$begin_month=strtrim(siteOnly$begin_month, 7)
            siteOnly$end_month=strtrim(siteOnly$end_month, 7)
            siteOnly=siteOnly[siteOnly$begin_month==target_bgn&siteOnly$end_month==target_end,]
        }

        key.cols="variable_tested"
        if(using.locs){
            key.cols=c("sensor_location", key.cols)
        }

        ref.params=unique(siteOnly[,colnames(siteOnly) %in% key.cols])
        ref.params=data.frame(na.omit(ref.params))


        for(r in 1:nrow(ref.params)){
                keyed.results=na.omit(merge(x = siteOnly, y = ref.params[r,], all.x = F, all.y = T))
                parsed.results=rbind(parsed.results, keyed.results[nrow(keyed.results),])

        }
       #varList = (unique(c(siteOnly$variable_tested)))
        #loc.list=unique(x = siteOnly$sensor_location)
        # for(v in 1:length(varList)){
        #     varOnly=siteOnly[which(siteOnly$variable_tested==varList[v]),]
        #     varOut=varOnly[which.max(rownames(varOnly)),]
        #     parsed.results=rbind(parsed.results, varOut)
        # }
    }
    if(write.summary==T){
        site=unique(parsed.results$site)

        summary.results=data.frame(cbind(site=unique(parsed.results$site),
                                         passed=unlist(
                                             lapply(site, function(x)
                                                 all(
                                                     parsed.results$data_quantity[parsed.results$site==x]>=
                                                         parsed.results$quant_threshold[parsed.results$site==x]
                                                 )&all(
                                                     parsed.results$data_validity[parsed.results$site==x]>=
                                                         parsed.results$valid_threshold[parsed.results$site==x])
                                             )
                                         ),
                                         bgn=unlist(lapply(site, function(x) unique(parsed.results$begin_month[parsed.results$site==x][1]))),
                                         end=unlist(lapply(site, function(x) unique(parsed.results$end_month[parsed.results$site==x])[1]))
        )
        )

        utils::write.csv(x = summary.results, file =  paste0(test.dir,"/Common/summary_results.csv"), row.names = F, append = F)


    }
    return(parsed.results)
}
