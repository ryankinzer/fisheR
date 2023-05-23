#' @title get_USACE_data
#'
#' @description Download data from the U.S. Army Corps of Engineers Dataquery 2.0 tool
#'
#' @author Tyler Stright
#'
#' @param query Desired Dataquery 2.0 query name. Visit https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/
#' to find additional queries than those provided.
#' @param startdate Beginning date of query range provided as 'MM/DD/YYYY'
#' @param enddate Final date of query range provided as 'MM/DD/YYYY'
#' @param timezone Desired Timezone (options: PST, PDT, MST, MDT, GMT)
#'
#' @source \url{https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/}
#'
#' @import httr
#' @export
#' @return NULL
#' @examples get_USACE_data(query = 'LWG.Power.Total.1Hour.1Hour.CBT-RAW',
#' startdate = '01/01/2022', enddate = '10/17/2022', timezone = 'PST')

get_USACE_data <- function(query = c('BON.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'TDA.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'JDA.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'MCN.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'IHR.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'LMN.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'LGS.Power.Total.1Hour.1Hour.CBT-RAW',
                                     'LWG.Power.Total.1Hour.1Hour.CBT-RAW'),
                           startdate,
                           enddate,
                           timezone = c('PST', 'PDT', 'MST', 'MDT', 'GMT')) {

  # Throw errors
  timezone = match.arg(timezone)

  {if(is.null(query))stop('A query name must be provided from: https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/ (Note: Do not include year range)')}
  {if(is.null(startdate) | !grepl('^\\d{2}/\\d{2}/\\d{4}$', startdate))stop("startdate must be provided as MM/DD/YYYY")}
  {if(is.null(enddate) | !grepl('^\\d{2}/\\d{2}/\\d{4}$', enddate))stop("enddate must be provided as MM/DD/YYYY")}
  {if(as.Date(enddate, format = '%m/%d/%Y') < as.Date(startdate, format = '%m/%d/%Y'))stop('startdate must be before enddate')}

  # get timezone correct for URL
  if(timezone == 'MDT'){timezone = 'CST'}
  if(timezone %in% c('MST', 'PDT')){timezone = 'MST'}
  if(timezone == 'PST'){timezone = 'PST'}
  if(timezone == 'GMT'){timezone = 'GMT'}

  # in queries these dates all have +06:00 in URL, regardless of timezone: MM/DD/YYYY+06:00
  startdate = paste0(startdate, '+06:00')
  enddate = paste0(enddate, '+06:00')

  query = paste0('["', query, '"]')

  # build URL
  base_url = 'https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/getjson'

  queryList <- list(timezone = timezone,
                    startdate = startdate,
                    enddate = enddate,
                    query = query)

  req_url <- httr::modify_url(base_url, query = queryList) # url encode

  req_url <- gsub('%2B06%3A00', '+06%3A00', req_url) # decode '+' symbol, which is + in the used URL.

  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query data from USACE'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  response <- jsonlite::fromJSON(req_con, flatten = TRUE)

  # extract timeseries, third column contains no information
  df <- as.data.frame(response[[1]][['timeseries']][[1]][['values']])[1:2]

  # extract parameter name and units
  param <- paste0(
    tolower(response[[1]][['timeseries']][[1]][['parameter']]),
    '_',
    tolower(response[[1]][['timeseries']][[1]][['units']])
  )

  # update field names, include timezone
  names(df) <- c('datetime', param)
  df$timezone = response[[1]][['timezone']]
  df$tz_offset = response[[1]][['tz_offset']]
  df$name = response[[1]][['name']]

  return(df)
}
