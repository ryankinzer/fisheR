#' @title get_biologic_sites
#' @description Pass API token to Biomark's Biologic database and receive a
#' vector of sites in return that the user has permissions.
#'
#' @author Ryan N. Kinzer
#'
#' @import httr
#' @return vector
#' @export
#'
#' @examples
#' biologic_login('email', 'password')
#' sites <- get_biologic_sites()
get_biologic_sites <- function(){

  # if (!exists("token", envir = .pkgglobalenv)) {
  #   cat("Token not found. Please log in first using biologic_login().")
  #   return(NULL)
  # }

  if(file.exists("biologic_token.rds")) {
    api_token <- readRDS("biologic_token.rds")
    if(api_token$expires > Sys.time()){
      token <- api_token$token
    } else {
      cat("Biologic API token has expired. Please log in again using biologic_login().")
      return(NULL)
    }
  } else {
    cat("Biologic API token not found. Please log in first using biologic_login().")
    return(NULL)
  }

  url <- 'https://data3.biomark.com/api/v1/authorized_sites/'

  req <- httr::GET(url,
                   httr::add_headers("content-Type"="application/json",
                                     Accept="application/json",
                                     "Authorization"=paste("Bearer", token)))

  if(req$status_code == 200){
    cat("Data was downloaded successfully.")
    # Extract and process the response data as needed
    data <- httr::content(req)
    sites <- unlist(data$sites)
    return(sites)
  } else {
    cat(paste("Unsuccessful -- requst returned status code:", req$status_code))
  }
}
