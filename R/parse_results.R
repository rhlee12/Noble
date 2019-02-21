############################################################################################
#' @title  Parse a Raw Commissioning Results File for Most Recent Results

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description Given a directory in the SCA, this function output only the most recent test results
#' from the 'results.csv' file in the
#'
#' @param test.dir The top level of a given test directory in the SCA (not the "Common" folder).
#' @param write.summary Logical, refering to if a simplified test summary should be written in
#' the Common folder of the specified directory. If TRUE, a summary_results.csv will be generated.
#'
#' @return The most recent test results from the 'results.csv' file in the

#' @keywords process quality, data quality, gaps, commissioning
# changelog and author contributions / copyrights
#   Robert Lee (2018-01-15)
#     original creation
#
##############################################################################################

parse.results=function(test.dir, write.summary=T){
    options(stringsAsFactors=FALSE)
    if(!is.logical(write.summary)){write.summary=TRUE}
    results = data.frame(utils::read.csv(.result.route(save.dir = test.dir), stringsAsFactors = FALSE))

    # Important! Only reads the most recent results data per site into the RMD #
    siteList = (unique(results$site))
    numbSites = as.numeric(length(siteList))


    results=results[with(results, order(site, time_performed, data_product)),]

    parsed.results=data.frame()

    for (k in 1:numbSites) {
        siteIndex=grep(pattern = siteList[k], results$site)
        siteOnly =results[siteIndex,]

        varList = (unique(siteOnly$variable_tested))

        for(v in 1:length(varList)){
            varOnly=siteOnly[which(siteOnly$variable_tested==varList[v]),]
            varOut=varOnly[which.max(as.POSIXct(varOnly$time_performed)),]
            parsed.results=rbind(parsed.results, varOut)
        }
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
