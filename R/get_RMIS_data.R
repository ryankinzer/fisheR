#' @title get_RMIS_data
#'
#' @description \code{get_RMIS_data} downloads data from the RMIS API returning
#' data for the specified agency and module. Restricted to returning a maximum
#' of 1000 records. If 1000 records are returned, more data should exist on
#' subsequent pages (Recommendation: increment your page argument).
#'
#' @param api_key unique access key obtained from RMIS.
#' @param agency desired return agency
#' @param module desired module to get data from
#' @param page desired page to return (default: 1)
#' @param perpage Number of records returned 1-1000 (default: 1000)
#'
#' @return data frame containing RMIS data from selected module
#' @import httr
#' @export
#'
# https://github.com/PSMFC-Streamnet-RMPC/api-docs
# https://phish.rmis.org/docs/static/index.html

get_RMIS_data <- function(api_key,
                          agency = c('NPT', 'ADFG', 'CCT', 'CDFO', 'CDFW',
                                     'CDFWKT', 'CRITFC', 'IDFG', 'NMFS',
                                     'NMFSNWR', 'NWIFC', 'ODFW', 'QDNR',
                                     'QUIL', 'RMPC', 'STIL', 'USFWS', 'WDFW',
                                     'YAKA', 'YTFP'),
                          module = c('release', 'location', 'files', 'recovery',
                                   'catchsample', 'description'),
                          page = 1,
                          perpage = 1000) {

  agency <- match.arg(agency)
  module <- match.arg(module)

  {if(is.null(api_key))stop('You must provide an api_key.')}
  {if(is.integer(page))stop('page must be an integer.')}
  {if(!is.integer(perpage)
      && perpage > 1000)stop('perpage must be an integer <= 1000.')}


  library(httr)

  url <- paste0("https://phish.rmis.org/", module)
  params <- list(
    page = page,
    perpage = perpage, # must be <=1000
    reporting_agency = agency
  )
  headers <- c(
    'xapikey' = api_key
  )

  req_url <- httr::modify_url(url, query = params)

  response <- GET(req_url,
                  add_headers(.headers = headers))

  if (status_code(response) == 200) {
    cat('Retrieving', agency, 'records from the RMIS', module, 'module...\n')
    content <- content(response, type = 'text', encoding = "UTF-8")

    r_json <- jsonlite::fromJSON(content, flatten = TRUE)

    df <- r_json$records

    if(nrow(df) == perpage) {
      cat('Returned data matches the "perpage" argument (', perpage, ').
        Additional records likely exist on the next page.\n')
    }

    return(df)
  } else {
    stop("Error:", status_code(response), "\n", content(response, "text"))
  }

}

