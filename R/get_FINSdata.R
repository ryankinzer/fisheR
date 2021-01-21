#' @title Download data from LSRCP FINS database.
#'
#' @description \code{get_FINSdata} downloads FINS data from their API using an
#'   NPT specific api key. The function can access data from three FINS modules;
#'   'Trapping', 'Release' and 'Spawning'. The user can also specify if only NPT
#'   data is required or the full dataset from FINS.
#'
#' @param module FINS database module to query; Trapping, Release, Spawning.
#' @param scope download NPT data only or the full FINS domain
#' @param startDate query start date formatted as month, day, year; '01/01/2019'
#' @param endDate query end date formatted as month, day, year; '01/01/2020'
#'
#' @return a data frame of all returned data from FINS query
#' @import httr
#' @export
#'
get_FINSdata <- function(module = c('Trapping', 'Release', 'Spawning'),
                            scope = c('NPT','FINS Domain'),
                            startDate, endDate){

  stopifnot(!is.na(startDate)|
            !is.na(endDate)|
            as.Date(endDate, "%m/%d/%Y") > as.Date(startDate, "%m/%d/%Y")
            )

  module <- match.arg(module)
  scope <- match.arg(scope)


  if(scope == 'FINS Domain'){
    scope = 'domain'
  }

  if(scope == 'NPT'){
    scope = NULL
  }

  # assign user agent to the GitHub repo for this package
  #ua = httr::user_agent('https://github.com/ryankinzer/cuyem')
  ua = httr::user_agent('Nez Perce Tribe')

  # compose url with query
  url_req = paste('https://www.finsnet.org/fins/ncg/', module, '/', sep='')


  # build query for FINS
  queryList = list(apikey = 'cGsvN7QlsSj0Uiy2kOLECQ8UsNhdt2bj',#apikey,
                   startDate = startDate,#
                   endDate = endDate,
                   scope = scope)#

  # send query to FINS
  httr::modify_url(url_req, query = queryList)
  web_req = httr::GET(url_req, ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from FINS')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  # parsed = httr::content(web_req, 'text')

  df = httr::content(web_req,
                         'parsed',
                         type = 'text/csv',
                         encoding = 'utf-8')

  #%>%
  # df <- readr::read_delim(parsed, delim = ',',
  #              col_names = T)

  if(is.null(df)) {
    message(paste('FINS returned no data'))
    stop
  }

  # if(class(parsed)[1] == 'xml_document') {
  #   message(paste('XML document returned by FINS instead of data\n'))
  #   stop
  # }

  if (httr::status_code(web_req) != 200) {
    message(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
    stop
  }

  return(df)
}

