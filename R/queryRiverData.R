#' @title Columbia River Daily Environmental Datea
#'
#' @description Query DART's river environment daily data.
#'
#' @author Ryan N. Kinzer and Kevin See
#'
#' @param site The site code to query for PIT tag data. Currently only available for Lower Granite Dam (LWG) and Bonneville (BON) forebays.
#' @param year Year to query. Available years include 1979-current for LWG and 1949-current for BON.
#' @param start_day Start date (\code{month / day}) of query. 
#' @param end_day End date (\code{month / day}) of query.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr tidyr
#' @export
#' @return NULL
#' @examples 
#' 
#' queryPITtagAdult(spawn_yr = 2015)

queryRiverData = function(site = c('LWG','LGS','LMN','IHR','MCN','JDA','TDA','BON'), 
                           year = NULL,
                           start_day = NULL,
                           end_day = NULL) {
  
  # need a year, and only dam allowed in Lower Granite (GRA)
  stopifnot(!is.null(year))
  
  # pull out default dam and species code
  site = match.arg(site)

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')
  
  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/river_daily.php'

  # build query for DART
  #?sc=1&outputFormat=csv&year=2017&proj=LWG&span=no&startdate=1%2F1&enddate=12%2F31
  queryList = list(sc = 1,
                   outputFormat = 'csv',
                   year = year,
                   proj = site,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day)
  
  # send query to DART
  web_req = httr::GET(url_req, ua,
                      query = queryList)
  
  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')
  
  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))
  
  # parse the response
  parsed = httr::content(web_req,
                         'text') %>%
    read_delim(delim = ',',
               col_names = T)
  
  if(is.null(parsed)) {
    message(paste('DART returned no data for', species, 'in', spawn_yr, '\n'))
    stop
  }
  
  if(class(parsed)[1] == 'xml_document') {
    message(paste('For', species, 'in', spawn_yr, 'XML document returned by DART instead of data\n'))
    stop
  }
  
  if (httr::status_code(web_req) != 200) {
    message(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
    stop
  }
  
  river_df = parsed %>%
    dplyr::mutate(Date = lubridate::ymd(Date)) %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::arrange(Date)
    
  names(river_df) <- c("Site", "Date", "Outflow", "Spill", "Spill_percent", "Inflow",
                       "Temp_scroll", "Temperature", "Barometric_Pressure", "Dissolved_Gas",
                       "TDG","Super_Saturation", "Turbidity", "Elevation")
  
  return(river_df)
}
