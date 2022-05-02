#' @title Return PTAGIS Site Information taken from DART
#'
#' @description Loads PTAGIS release site or observation site information
#'
#' @author Tyler Stright
#'
#' @param site_type Desired PTAGIS site type: observation or release
#'
#' @import
#' @export
#' @return NULL
#' @examples
#'
#' DART_sites('release')

DART_sites <- function(site_type = c('observation', 'release')) {

  site_type <- match.arg(site_type, several.ok = FALSE)

  if(site_type == 'observation') {
    load('./data/observation_sites.rda')

    sites_df <- obs_sites
  }

  if(site_type == 'release') {
    load('./data/release_sites.rda')

    sites_df <- rel_sites
  }

  return(sites_df)
}
