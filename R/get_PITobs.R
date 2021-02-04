#' @title DART PIT tag Observation Data
#'
#' @description Query DART's PIT tag observation detail records.
#'
#' @author Ryan N. Kinzer
#'
#' @param query_type Query PIT-tag observation data for either an observation site (default) or
#' a release site.
#' @param obs_site For use with observation site query. Currently only available for Lower Granite Dam (GRA, default) and Bonneville (B2A).
#' @param release_site For use with release site query. Use PTAGIS release codes.
#' @param species Species to query. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param run Run to query. Possible choices are: All, Spring, Summer, Fall, Winter, Unknown
#' @param rear_type Rear type to query. Possible choices are: All, W+H, Wild, Hatchery, Unknown
#' @param start_date Observation start date with format mm/dd/yyyy
#' @param end_date Observation end date with format mm/dd/yyyy
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import httr lubridate
#' @export
#' @return NULL
#' @examples
#'
#' queryPITtagAdult(spawn_yr = 2015)

get_PITobs = function(query_type = c('obs_site', 'release_site'),
                         obs_site = c('GRA','B2A','GRJ'), # many others
                         release_site = NULL,
                         species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                         run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                         rear_type = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                         start_date = NULL,
                         end_date = NULL) {

  # pull out default params and check for problems
  query_type <- match.arg(query_type)

  stopifnot(!is.null(start_date)|!is.null(end_date))
  stopifnot(grepl('\\d{2}/\\d{2}/\\d{4}', start_date)|grepl('\\d{2}/\\d{2}/\\d{4}', end_date))

  #stopifnot(!is.null(obs_year), obs_year >= 1989) #spawn_yr = NULL, #1988 to current for GRA and 1995 for B2A

  obs_site <- match.arg(obs_site, several.ok = FALSE)
  species <- match.arg(species, several.ok = FALSE)
  run <- match.arg(run, several.ok = FALSE)
  rear_type <- match.arg(rear_type, several.ok = FALSE)

  # set up default start and end days
  start_day <- substr(start_date,1,5)
  syear <- substr(start_date, 7,10)

  end_day <- substr(end_date,1,5)
  eyear <- substr(end_date, 7,10)

  {if(as.numeric(eyear) - as.numeric(syear) > 1) stop("year range must be less than 2 years")}

  if(as.numeric(eyear) - as.numeric(syear) == 0){
      span <- 'no'
    } else {
      span <- 'yes'
    }

  # match up site code with site name
  obs_code_df <- data.frame(site = c('GRA', 'B2A', 'GRJ'),
                             code = c('GRA:Lower Granite Dam Adult Fishway (GRA) rkm 522.173',
                                      'B2A:Bonneville Dam Adult Fishways (B2A BO1 BO2 BO3 BO4 BWL) rkm 234',
                                      'LWG:Lower Granite Juvenile (GRJ GRS) rkm 522.173'))

  obs_code = obs_code_df$code[match(obs_site, obs_code_df$site)]

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  spp_code = spp_code_df$code[match(species, spp_code_df$Species)]

  # match up run code with run name
  run_code_df = data.frame(Run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                           code = c('Null',1:5))

  run_code = run_code_df$code[match(run, run_code_df$Run)]

  # match up rear type code with rear type name
  rear_code_df = data.frame(Rear = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                            code = c('Null', 'WH','W', 'H', 'U'))

  rear_code = rear_code_df$code[match(rear_type, rear_code_df$Rear)]

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')

  # build query for DART
  if(query_type == 'obs_site'){

    # compose url with query
    url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php'

    queryList = list(sc = 1,
                   queryName = 'pitadult_obs_de',
                   stage = NULL,
                   outputFormat = 'csv',
                   year = syear,
                   proj = obs_code,
                   species = spp_code,
                   run = run_code,
                   rear_type = rear_code,
                   span = span,
                   startdate = start_day,
                   enddate = end_day,
                   syear = syear,
                   eyear = eyear,
                   reltype = 'alpha',
                   relloc = NULL,
                   summary = 'no')
  }

  if(query_type == 'release_site'){

    # compose url with query
    url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_rel_de.php'

    queryList = list(sc = 1,
                     #queryName = 'pitadult_obs_de',
                     #stage = NULL,
                     outputFormat = 'csv',
                     year = syear,
                     rel_site = release_site,
                     species = spp_code,
                     run = run_code,
                     rear_type = rear_code,
                     span = span,
                     startdate = start_day,
                     enddate = end_day,
                     #reltype = 'alpha',
                     #relloc = NULL,
                     syear = syear,
                     eyear = eyear,
                     summary = 'no')
  }

  httr::modify_url(url_req, query = queryList)


  # May need to include something like the following if we span years or look for
  # steelhead spawn years.
  # if(grepl('Steelhead', species)) {
  #   queryList[['span']] = 'yes'
  #   queryList[['year']] = spawn_yr -1
  #   queryList = c(queryList,
  #                 list(syear = spawn_yr - 1,
  #                      eyear = spawn_yr))
  # }


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

  names(parsed) <- gsub(' ','_',tolower(names(parsed)))

  # if(dim(parsed)[2]==1) {
  #   message(paste('DART returned no data for', species, 'in', spawn_yr, '\n'))
  #   stop
  # }

  if(is.character(as.data.frame(parsed)[,1])){  #any(parsed[,1]=='tag_id'))
    # this will properly skip lines if needed.
    parsed = httr::content(web_req,'text') %>%
      read_delim(delim = ',', col_names = T, skip = which(parsed[,1]=='tag_id'))
  }

  if(nrow(parsed)>=100000 && query_type=='obs_site'){
    parsed = parsed %>%
      mutate(sprrt = paste0(t_species, t_run, t_rear_type)) %>%
      select(year, tag_file = file_id, nfish = srrt_cnt, tag_id, sprrt, length, mark_site, release_site = rel_site, total_rkm = total_km,
             release_date = rel_date, obs_site, obs_time, stage, travel_days = tt, min_time, max_time)
    # change names.
  }

  dat <- parsed[!is.na(parsed$tag_id),] # need to remove empty rows with metadata and bottom header row
  dat <- dat[dat$tag_id != 'Tag ID',]

  # add life stage
  dat <- dat %>%
    mutate(
    stage = gsub(' ','',stage),
    sprrt = ifelse(nchar(sprrt) == '2',paste0(spp_code,sprrt),sprrt),
    species =  case_when(  #sprrt is only 3 characters long with obs dat and 2 characters with releae_dat.
      substr(sprrt,1,1) == '1' ~ 'Chinook salmon',
      substr(sprrt,1,1) == '2' ~ 'Coho salmon',
      substr(sprrt,1,1) == '3' ~ 'Steelhead',
      substr(sprrt,1,1) == '4' ~ 'Sockeye salmon',
      TRUE ~ 'Unknown'
    ),
    run =  case_when(
      substr(sprrt,2,2) == '1' ~ 'Spring',
      substr(sprrt,2,2) == '2' ~ 'Summer',
      substr(sprrt,2,2) == '3' ~ 'Fall',
      substr(sprrt,2,2) == '4' ~ 'Winter',
      substr(sprrt,2,2) == '5' ~ 'Unknown',
      TRUE ~ 'Unknown'
    ),
    rear =  case_when(
      grepl('W', sprrt) ~ 'Natural',
      grepl('H', sprrt) ~ 'Hatchery',
      TRUE ~ 'Unknown'
    ),
    obs_time = lubridate::ymd_hms(obs_time),
    release_date = lubridate::ymd(release_date),
    release_year = lubridate::year(release_date),
    min_time = lubridate::ymd_hms(min_time),
    max_time = lubridate::ymd_hms(max_time),
    travel_days = as.numeric(travel_days),
    length = as.numeric(length),
    tag_season = case_when(
      lubridate::month(release_date) <= 6 ~ 'Spring',
      between(lubridate::month(release_date),7,8) ~ 'Summer',
      lubridate::month(release_date) >= 9 ~ 'Fall',
      TRUE ~ 'Unknown'
    )
    )

  if(query_type == 'release_site'){
    dat <- dat %>%
      mutate(total_rkm = map(site_rkm, function(x){  # site_rkm only in release dat total_rkm already exists
        str_split(x, '\\.', simplify = TRUE) %>%
          as.numeric() %>%
          sum()
      })
      ) %>%
      unnest(cols = total_rkm)
  }

  return(dat)
}
