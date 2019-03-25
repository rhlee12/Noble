############################################################################################
#' @title  Return the Current Sensor Location Information for a Given Data Product

#' @author Robert Lee \email{rlee@battelleecology.org}\cr

#' @description For a given data product ID and site, a data frame of sensor location
#' information is returned.
#'
#' @param site Parameter of class character. The 4-letter site code for the NEON
#'  site of interest.
#' @param dp.id Parameter of class character. The data product ID of interest,
#' must be in code format, eg. "DP1.00001.001"
#'
#' @return A data frame of location information for sensors used in the data product
#' generation.
#'
#' @keywords spatial, meta data, sensor, position

#' @examples
#' \dontrun{
#' 2d_wind_locs=Noble::pull.dp.locs(site="CPER", dp.id="DP1.00001.001")
#'}
#' @seealso \itemize{
#' \item\code{\link{neon.avail}}, which returns a data frame of data product availability by site and month.
#' \item\code{\link{test.sites}}, which produces a list of sites that have a given data product installed.
#' }
#'

# changelog and author contributions / copyrights
#   Robert Lee (2018-04-26)
#     original creation
#
##############################################################################################

pull.dp.locs=function(site, dp.id){
    options(stringsAsFactors=F)
    month=format(zoo::as.yearmon(Sys.Date()-lubridate::weeks(8)), "%Y-%m")
    out=jsonlite::read_json(path=paste0("http://data.neonscience.org/api/v0/data/", dp.id, "/", site, "/", month))
    files=unlist(lapply(out$data$files, "[[", "name"))
    loc.url=out$data$files[[grep(pattern = "sensor", x = files)[1]]]$url
    loc.info=utils::read.csv(file = loc.url)
    raw.hor.vers=stringr::str_split(loc.info$HOR.VER, "\\.")

    loc.parse=function(x){
        if(length(x)==1){
            if(x==9){
                out=paste0(x, "00", ".000")
            }else if(!x==9){
                paste0("00", x, ".000")
            }
        }else if(length(unlist(x))==2){
            if(grepl(unlist(x)[2], pattern = "^5")){
                out=paste0( "00", unlist(x)[1],".", unlist(x)[2])
            }else if(!grepl(unlist(x)[2], pattern = "^5")){out=paste0(unlist(x)[1],"00.", unlist(x)[2], "0")}
        }
    }

    formed.hor.vers=lapply(raw.hor.vers, function(x) loc.parse(x))
    loc.info$HOR.VER=unlist(formed.hor.vers)

    return(loc.info)
}
