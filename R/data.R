#' @name ais_site_config
#' @docType data
#' @title Site Configuration for NEON AIS Stations
#' @description AIS site configuration and metadata, culled from internal documentation. Lists site
#' name, domain, type, and data product availabilty.
#' @keywords AIS, metadata, configuration
#' @usage ais_site_config
#' @format Information on all 36 AIS sites in table/data frame format.
NULL
#' @name USCRN_sites
#' @docType data
#' @title USCRN Site List
#' @description A list of USCRN stations. Parsed from
#' ghcnd-stations.txt, available on the NCDC/NCEI's FTP server at
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt}.
#' @keywords USCRN, GHCND, NCDC, NCEI, metadata
#' @usage USCRN_sites
#' @format A data frame/table listing site metadata for 242 USCRN sites.
NULL
#' @name is_site_config
#' @docType data
#' @title Site Configuration for NEON IS Stations
#' @description A merged metadata set of TIS and AIS sites, culled from internal documentation.
#' Lists site name, domain, type, and data product availabilty. Information in the ais_site_config and
#' tis_site_config databases was discarded to merge the two databases, so it's recommended that the
#' AIS- or TIS-specific database be used instead where possible.
#' @keywords TIS, AIS, metadata, configuration
#' @usage is_site_config
#' @format Information on all 81 IS sites in table/data frame format.
NULL
#' @name rad_dq_info
#' @docType data
#' @title 'Hard-coded' TIS Site Information for Radiation DQ Commissioning.
#' @description Data used in TIS radiation data quality commissioning, collected in a database for
#' convenience.
#' @keywords TIS, radiation, SCAN, USCRN, metadata, configuration
#' @usage rad_dq_info
#' @format Information for 9 sites tested for Radiation DQ commissioning in table/data frame format.
NULL
#' @name tis_pri_vars
#' @docType data
#' @title TIS Primary Variable database
#' @description Information on selected data products, such as the data product name, number, primary
#' variable reported, units, and commissioning info.
#' @keywords TIS, data products, units, metadata, configuration
#' @usage tis_pri_vars
#' @format Information for 16 TIS data products in table/data frame format.
NULL
#' @name tis_site_config
#' @docType data
#' @title Site Configuration for NEON AIS Stations
#' @description TIS site configuration and metadata, culled from internal documentation. Lists site
#' name, domain, type, lat/long, timezone, and data product availabilty.
#' @keywords TIS, metadata, configuration
#' @usage tis_site_config
#' @format Information on all 48 TIS sites in table/data frame format.
NULL
#' @name wind_thresholds
#' @docType data
#' @title ATBD Specific Thresholds: 2D Wind Speed and Direction
#' @description A theshold file for the 2D Wind Speed and Direction data product (DP1.00001.001), used
#' by NEON CI to generate flags and quality metrics. Used in Noble to analyze data quality for
#' 2D Wind Speed and Direction.
#' @keywords TIS, 2D Wind Speed and Direction, metadata, thresholds, flag, quality
#' @usage wind_thresholds
#' @format Thresholds and tower information for 2D wind at all IS sites in table/data frame format.
NULL
