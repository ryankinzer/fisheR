#' @title biologic_login
#' @description Login into Biomark's Biologic database to retreive an API token.
#'
#' @author Ryan N. Kinzer
#'
#' @param email User's email address attached to Bioilogic account.
#' @param password Password for user's Biologic account.
#'
#' @import httr
#' @return
#' @export
#'
#' @examples
#' biologic_login('email', 'password')
biologic_login <- function(email, password){

  stopifnot(!is.null(email),
            !is.null(password))

  endpoint <- 'https://data3.biomark.com/api/v1/token/'
  creds <- list(email = email, password = password)
  req <- httr::POST(endpoint, body = creds, encode = 'json')

  if(req$status_code == 200){
    cat("Login was successful.")
    api_token <- list(token = httr::content(req)$access,
                      expires = Sys.time() + 15*60)
    saveRDS(api_token, "biologic_token.rds")
    return(invisible(TRUE))
  } else {
    cat(paste("Unsuccessful login with status code:", req$status_code))
    return(invisible(FALSE))
  }
}
