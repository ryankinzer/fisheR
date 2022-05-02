#' @title DART PIT Tag Adult Returns by Observation Year Detail
#'
#' @description Query DART's adult PIT tag observation detail records for an observation year..
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @param obs_site PTAGIS code for desired observation site. see fisheR::DART_sites()
#' @param species Species to query. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param run Run to query. Possible choices are: All, Spring, Summer, Fall, Winter, Unknown
#' @param rear_type Rear type to query. Possible choices are: All, W+H, Wild, Hatchery, Unknown
#' @param start_date Observation start date with format mm/dd/yyyy
#' @param end_date Observation end date with format mm/dd/yyyy
#'
#' @source \url{https://www.cbr.washington.edu/dart/query/pitadult_obsyr_detail}
#'
#' @import httr lubridate
#' @export
#' @return NULL
#' @examples
#'
#' DART_AdultReturns_obsyr(obs_site = 'BON', species = 'Coho', run = 'All', rear_type = 'All',
#'                         start_date = '02/01/2020', end_date = '02/01/2021')

DART_AdultReturns_obsyr = function(obs_site,
                                   species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                                   run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                                   rear_type = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                                   start_date,
                                   end_date) {

  # pull out default params and check for problems
  if(is.null(obs_site))stop('You must provide a PTAGIS observation site code (obs_site). see DART_sites("observation")')
  stopifnot(!is.null(start_date)|!is.null(end_date))
  stopifnot(grepl('\\d{2}/\\d{2}/\\d{4}', start_date)|grepl('\\d{2}/\\d{2}/\\d{4}', end_date))

  # obs_site <- match.arg(obs_site, several.ok = FALSE)
  species <- match.arg(species, several.ok = FALSE)
  run <- match.arg(run, several.ok = FALSE)
  rear_type <- match.arg(rear_type, several.ok = FALSE)

  # set up default start and end days
  start_day <- substr(start_date,1,5)
  start_year <- substr(start_date, 7,10)

  end_day <- substr(end_date,1,5)
  end_year <- substr(end_date, 7,10)

  if(as.numeric(end_year) - as.numeric(start_year) > 1) stop("year range must be less than 2 years")

  if(as.numeric(end_year) - as.numeric(start_year) == 0){span <- 'no'} else {span <- 'yes'}

  # Character -> Code Transcription
  source('./R/DART_codes.R')
  codes <- DART_codes(obs_site, species, run, rear_type, life_stage = 'Adult')

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')

  # build query for DART
    url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php'

    queryList = list(sc = 1,
                     queryName = 'pitadult_obs_de',
                     stage = codes[5], # 'A', # life_stage static.
                     outputFormat = 'csv',
                     year = start_year,
                     proj = codes[1], #obs_code,
                     species = codes[2], #spp_code,
                     run = codes[3], #run_code,
                     rear_type = codes[4], #rear_code,
                     span = span,
                     startdate = start_day,
                     enddate = end_day,
                     syear = start_year,
                     eyear = end_year,
                     reltype = 'alpha',
                     relloc = NULL,
                     summary = 'no')

  httr::modify_url(url_req, query = queryList)

  # send query to DART
  web_req = httr::GET(url_req, ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  parsed = suppressMessages(
    suppressWarnings(
      httr::content(web_req, 'text') %>%
        read_delim(delim = ',', col_names = T)
    )
  )

  if(ncol(parsed)==1 && grepl('No PIT Tag Adult Returns Observation Detail data found', parsed[,1]))
    stop('No PIT Tag Adult Returns Observation Detail data found for provided parameters.')

  names(parsed) <- gsub(' ','_',tolower(names(parsed)))

  if(is.character(as.data.frame(parsed)[,1]) && names(parsed)[[1]]!='year'){  #any(parsed[,1]=='tag_id'))
    # this will properly skip lines if needed.
    parsed = httr::content(web_req,'text') %>%
      read_delim(delim = ',', col_names = T, skip = which(parsed[,1]=='tag_id'))
  }

  if(nrow(parsed)>=100000){
    parsed = parsed %>%
      mutate(sprrt = paste0(t_species, t_run, t_rear_type)) %>%
      select(year, tag_file = file_id, nfish = srrt_cnt, tag_id, sprrt, length, mark_site, release_site = rel_site, total_rkm = total_km,
             release_date = rel_date, obs_site, obs_time, stage, travel_days = tt, min_time, max_time)
    # change names.
  }

  dat <- parsed[!is.na(parsed$tag_id),] # need to remove empty rows with metadata and bottom header row
  dat <- dat[dat$tag_id != 'Tag ID',]

  # clean returned data
  source('./R/clean_DART.R')
  dat <- clean_DART(dat)

  return(dat)
}
