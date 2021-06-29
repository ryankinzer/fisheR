#' @title PIT tag Adult Return Data
#'
#' @description Query DART's PIT tag adult return observation detail.
#'
#' @author Ryan N. Kinzer and Kevin See
#'
#' @param dam The dam code to query for PIT tag data. Currently only available for Lower Granite Dam (GRA) and Bonneville (B2A).
#' @param spp Species to query. for window counts. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param spawn_yr Spawn year to query. Available years include 1988-current for GRA and 1995-current for B2A.
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

queryPITtagAdult = function(site = c('GRA','B2A'), # many others
                           species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                           rear_type = c('W+H', 'Wild', 'Hatchery', 'Unknown'),
                           spawn_yr = NULL, #1988 to current for GRA and 1995 for B2A
                           start_day = NULL,
                           end_day = NULL) {
  
  # need a year, and only dam allowed in Lower Granite (GRA)
  stopifnot(!is.null(spawn_yr), spawn_yr >= 1989)
  
  # pull out default dam and species code
  site = match.arg(site)
  species = match.arg(species, several.ok = F)
  run = match.arg(run, several.ok = F)
  rear_type = match.arg(rear_type, several.ok = F)
  
  # set up default start and end days
  if(site == 'B2A' & species == 'Chinook' & is.null(start_day)) start_day = '03/01'
  if(site == 'B2A' & species == 'Chinook' & is.null(end_day)) end_day = '07/31'
  
  if(site == 'B2A' & grepl('Steelhead', species) & is.null(start_day)) start_day = '07/01'
  if(site == 'B2A' & grepl('Steelhead', species) & is.null(end_day)) end_day = '06/30'
  
  if(site != 'B2A' & species == 'Chinook' & is.null(start_day)) start_day = '03/01'
  if(site != 'B2A' & species == 'Chinook' & is.null(end_day)) end_day = '08/17'
  
  if(site != 'B2A' & grepl('Steelhead', species) & is.null(start_day)) start_day = '07/01'
  if(site != 'B2A' & grepl('Steelhead', species) & is.null(end_day)) end_day = '06/30'
  
  # match up site code with site name
  site_code_df <- data.frame(site = c('GRA', 'B2A'),
                             code = c('GRA:Lower Granite Dam Adult Fishway (GRA) rkm 522.173',
                                      'B2A:Bonneville Dam Adult Fishways (B2A BO1 BO2 BO3 BO4 BWL) rkm 234'))
  
  site_code = site_code_df$code[match(site, site_code_df$site)]
  
  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)
  spp_code = spp_code_df$code[match(species, spp_code_df$Species)]
  
  # match up run code with run name
  run_code_df = data.frame(Run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                           code = c('Null',1:5))
  
  run_code = run_code_df$code[match(run, run_code_df$Run)]

  # match up rear type code with rear type name
  rear_code_df = data.frame(Rear = c('W+H', 'Wild', 'Hatchery', 'Unknown'),
                           code = c('WH','W', 'H', 'U'))
  
  rear_code = rear_code_df$code[match(rear_type, rear_code_df$Rear)]  
  
  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')
  
  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php'

  # build query for DART
  queryList = list(sc = 1,
                   queryName = 'pitadult_obs_de',
                   stage = 'A',
                   outputFormat = 'csv',
                   year = spawn_yr,
                   proj = site_code,
                   species = spp_code,
                   run = run_code,
                   rear_type = rear_code,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day,
                   reltype = 'alpha',
                   relloc = NULL,
                   summary = 'no')
  
  if(grepl('Steelhead', species)) {
    queryList[['span']] = 'yes'
    queryList[['year']] = spawn_yr -1
    queryList = c(queryList,
                  list(syear = spawn_yr - 1,
                       eyear = spawn_yr))
  }
  
  
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
  
  pit_df = parsed %>%
    dplyr::mutate(`Release Date` = lubridate::ymd(`Release Date`),
                  `Obs Time` = lubridate::ymd_hms(`Obs Time`),
                  Return_Year = year(`Obs Time`),
                  Spawn_Year = spawn_yr) %>%
    dplyr::filter(!is.na(`Release Date`)) %>%
    dplyr::arrange(`Release Site`, `Release Date`)

  
  names(pit_df) = gsub(' ', '_', names(pit_df))
  
  pit_df = pit_df %>%
    dplyr::select(-one_of('Year')) %>%
    dplyr::select(Return_Year, Spawn_Year, everything())
  
  return(pit_df)
}
