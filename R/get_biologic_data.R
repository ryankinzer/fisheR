#' @title get_biologic_data
#' @description Pass API token to Biomark's Biologic database and receive a site
#' data.
#'
#' @author Ryan N. Kinzer
#'
#' @param site IPTDS three character site code.
#' @param endpoint The type of data you want returned.
#' @param begin_dt Start date for the data returned.
#' @param end_dt End date for the data returned.
#'
#' @import httr data.table
#' @return data.frame
#' @export
#'
#' @examples
#' biologic_login('email', 'password')
#' data <- get_biologic_data('KRS', 'tags', '2023-06-01', '2023-06-02')
get_biologic_data <- function(site = NULL,
                              endpoint = c('tags', 'reader', 'enviro', 'antenna'),
                              begin_dt = NULL, end_dt = NULL){

  if(file.exists("biologic_token.rds")) {
    api_token <- readRDS("biologic_token.rds")
    if(api_token$expires > Sys.time()){
      token <- api_token$token
    } else {
      cat("Biologic API token has expired. Please log in again using biologic_login().\n")
      return(NULL)
    }
  } else {
    cat("Biologic API token not found. Please log in first using biologic_login().\n")
    return(NULL)
  }

  endpoint <- match.arg(endpoint)

  stopifnot(!is.null(site),
            !is.null(begin_dt),
            !is.null(end_dt))

  url <- paste0('https://data3.biomark.com/api/v1/',endpoint,'/',site,'/')

  params = list(begin_dt = begin_dt,
                end_dt = end_dt)

  req <- httr::GET(url,
                   httr::add_headers("content-Type"="application/json",
                                     Accept="application/json",
                                     "Authorization"=paste("Bearer", token)),
                   query = params)

  if(req$status_code == 200){
    cat("Data was downloaded successfully.\n")
    # Extract and process the response data as needed
    data <- httr::content(req)
    dt <- data.table::rbindlist(lapply(data, function(x) data.table::as.data.table(t(unlist(x, recursive = TRUE)))))
    return(dt)
  } else {
    cat(paste("Unsuccessful -- data requst returned status code:", req$status_code, '\n'))
  }
}
