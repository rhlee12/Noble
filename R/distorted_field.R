#' @title Distorted Field Calculations
#' @author Robert Lee \email{rlee@battelleecology.org}\cr
#'
#' @description Produces the boundaries of the distorted flow fields for 2D Wind Speed and Direction at
#' TIS sites
#' @param site The NEON TIS site of interest
#'
#' @return A list of distorted field mins and maxes, as well as the corresponding upper and lower
#' buffer field mins and maxes.
#'
#' @keywords 2D wind, distorted flow, data quality
#'
#' @examples
#' \dontrun{
#' out=distorted.field("CPER")
#' }

#' @export
#'
# # changelog and author contributions / copyrights
#   Robert Lee (2017-12-05)
#     original creation
#
##############################################################################################

distorted.field=function(site){
    thresholds=Noble::wind_thresholds

    y_c=thresholds$dy_clockwise[thresholds$SITE==site] #
    y_cc=thresholds$dy_counterClockwise[thresholds$SITE==site] #
    L=thresholds$boomLength[thresholds$SITE==site] #
    x_c=thresholds$dx_clockwise[thresholds$SITE==site] #
    x_cc=thresholds$dx_counterClockwise[thresholds$SITE==site] #

    O_2D = thresholds$boomOrientation[thresholds$SITE==site] # Boom orientation
    B_min = thresholds$Distorted.Flow.Min.Threshold[thresholds$SITE==site] # min distorted flow angle
    B_max = thresholds$Distorted.Flow.Max.Threshold[thresholds$SITE==site] # max distorted flow angle


    #### Wind Field Calculations
    c_d=(abs(atan(y_c/(L+abs(x_c))))*180)/pi # Eqn. 22
    cc_d=(abs(atan(y_cc/(L+abs(x_cc))))*180)/pi # Eqn. 23


    #### Direction thresholds
    D_min = round((O_2D-(c_d +B_min)+180)%%360, digits = 3) # Eqn 24
    D_max = round((O_2D+(cc_d +B_max)+180)%%360, digits = 3) # Eqn 25

    buff_low=c((D_min-B_min)%%360, D_min) # Bracketing the low end of distorted flow
    buff_hi = c(D_max, (D_max+B_max)%%360) # Bracketing the high end of distorted flow

    DF.info=list(distortedField=c(D_min, D_max), buff_low=buff_low, buff_hi=buff_hi)

    return(DF.info)}
